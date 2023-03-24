# trace-dispatcher: efficient, simple and flexible program tracing

`trace-dispatcher` is a simple, efficient __tracing library__, that is based on the contravariant tracing library and should use little resources in the executed program, has a pleasant API, and provides self-documenting features.

`trace-dispatcher` consists of generic combinators, which are then used in functions specific for the cardano application.

## Contents

1. [Contents](#contents)
2. [Introduction](#Introduction)
   1. [Motivation](#motivation)
   2. [Design decisions](#Design-decisions)
   3. [Terminology](#Overview-and-terminology)
3. [Overview](#overview)
4. [Formatting](#formatting)
    1. [Detail level](#detail-level)
    2. [Metrics](#metrics)
5. [MetaTrace](#metatrace)
    1. [Trace namespace](#trace-namespace)
    2. [Severity](#severity)
    3. [Privacy](#privacy)
    4. [Detail level again](#detail-level-again)
    5. [Documentation](#documentation)
6. [Cardano tracer](#cardano-tracer)
7. [Configuration](#configuration)
    1. [Configuring Severity](#configuring-severity)
    2. [Configuring Detail Level](#configuring-detail-level)
    3. [Configuring Frequency Limiting](#configuring-frequency-limiting)
    4. [Configuring Backends](#configuring-backends)
8. [Reconfiguration](#reconfiguration)
9. [Special tracers](#special-tracers)
    1. [Hook]
    2. [Fold-based aggregation](#fold-based-aggregation)
    3. [Trace routing](#trace-routing)
10. [Documentation generation](#documentation-generation)
11. [DataPoints](#datapoints)
12. [References](#references)

## Introduction

### Motivation

`trace-dispatcher` is an implementation of simple, efficient __tracing systems__, one that has a reduced footprint in the executed program, has a more pleasant API, and provides self-documenting features.

For a quick start into new tracing see the document
[New Tracing Quickstart](https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/New%20Tracing%20Quickstart.md)

## Design decisions

Key design decisions were:

1. Retaining the separation of concerns in the frontend side, as provided by the `contra-tracer` library.  The client code should not need to concern itself with any details beyond passing the traces down to the system.
2. Rely on __trace combinators__ primarily, as opposed to opting for a typeclass heavy API.
3. Separation of data plane and control plane:  high-frequency events of the data-plane (corresponding to actual trace emission), whereas complicated configuration-induced logic of the control plane is proportional to infrequent reconfiguration events.  This is the principle we tried to ensure across the system -- and hopefully succeeded to a reasonable degree.
4. A tougher stance on separation of concerns in the backend side:  we choose to move expensive trace processing to an external process, which is called cardano-tracer.
5. A measure of backward compatibility with the previous logging system.
6. Retaining a global namespace for all traces.

### Terminology

The emitted __program traces__ (streams of __messages__ of arbitrary data types, where each data type defines a number of different __messages__) are collected across all program components, and undergo __trace interpretation__ by the __dispatcher__ into __metrics__ and __messages__, which are afterwards externalised.

Therefore, we can conceptually decompose the __tracing system__ into three components:

* __frontend__, the entry point for __program trace__ collection, which is just a single function `traceWith`;  Program locations that invoke this frontend (thereby injecting messages into the tracing system) is called __trace-ins__.
* __dispatcher__, is a structured, namespaced set of contravariantly-composed transformations, triggered by the entry point.  Its role is specifically __trace interpretation__;
* __backend__, externalises results of the interpretation ( __metrics__ and __messages__) outside the tracing system, through __trace-outs__.

The trace-emitting program itself is only exposed to the the frontend part of the tracing system, as it only needs to define the traces themselves, and specify the __trace-ins__ -- call sites that inject traces.  It is notably free from any extra obligations, such as the need to define the `LogFormatting` instances.

As mentioned above, __dispatcher__ is the point of interpretation of the program traces -- a structured set of __Tracer__ objects, that defines and implements the __language and policy__ of __trace interpretation__.

__Trace interpretation__ is specified in terms of:

* __trace synthesis__, which means production of __synthetic traces__ -- in cases where we decide it is cheaper (or at all possible) to perform trace aggregation inside the program,
* __trace naming__, which is assignment of hierarchically-structured names to all traces -- which serve identification, documentation and configuration purposes,
* __trace filtering__: which, in turn relies on notions of __severity__, __privacy__ and __frequency__ of messages,
* __trace presentation__ : relying on __detail level__ and on the `LogFormatting` transformation of the traces into JSON, human readable and metric forms -- the last step before traces meet their __trace-outs__,
* __trace documentation__, as a mode of the __dispatcher__ operation.

The __trace interpretation__ process requires that for each traced type the __dispatcher__ is provided with:

* instances of `MetaTrace` typeclass, to provide a namespace and a severity and optionally privacy and detail level.
  In addition a __trace documentation__ for trace messages and metrics needs to be provided.
  Finally all namespaces for a type needs to be specified.
* instances of the `LogFormatting` typeclass, to specify different formatting possibilities.

__Trace interpretation__ would have been unusably static, if it wasn't allowed to be configured without recompilation -- and therefore the __effective tracing policy__ used by the __dispatcher__ can be defined by the externally-supplied __trace configuration__.

The __effective tracing policy__ defines for each trace a __trace context__, which is what effectively informs interpretation performed by the __dispatcher__ for that particular trace.

The __trace context__, in turn, consists of the __logging context__ encoded in the __dispatcher__, and the __configuration context__ coming from the __trace configuration__.

As a final note, the __dispatcher__ is not provided by the `trace-dispatcher` library as a ready-made, turn-key component -- instead, we are provided with __trace combinators__, the building blocks that allow its construction -- and therefore, expression of the desirable __trace interpretation policies__.


## Overview

In this overview we shortly touch a lot of aspects, which will be explained in more detail later.

As we use the contravariant tracer library as basics you need to familiarize yourself
with this concept. Basically contravariant means that the flow is turned around from what you have normally. The tracer input to a function is at the result position, while the output is the argument. As well `do` expressions which input and output tracers have to be read from bottom to top.

__Traces__ begin with a definition of a datatype with __messages__. E.g. `TraceAddBlockEvent` is the datatype and `IgnoreBlockOlderThanK` a message:

```haskell
data TraceAddBlockEvent blk =
    IgnoreBlockOlderThanK (RealPoint blk)
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)
  ...
```

Trace messages are either discarded or converted to a representation quickly. As the discarded case happens frequently and trace messages are never stored, strictness annotations are of no use, and can only make the runtime behavior worse.

As the name __Tracer__ is used by the underlying contravariant library, we use the name __Trace__ in our library.

The data types of trace messages need to be instances of the following two Typeclasses:

* the `MetaTrace` typeclass, to provide meta information of the messages. Most importantly __Traces__ are organised into a hierarchical __tracer namespace__, where the components are `Text`.

* instances of the `LogFormatting` typeclass, to specify transformations to output.

To trace a message of a type like TraceAddBlockEvent we need to construct a matching __Trace__ TraceAddBlockEvent. The `mkCardanoTracer` function is doing this for us.
The `mkCardanoTracer` gets as arguments the trace backends: `trStdout`, `trForward` and `mbTrEkg`, and a composed `name`, which is prepended to its namespace.

Before the trace can be used we need to configure it with calling the configureTracers
function with the tracers and an configuration.

To actually emit a trace, given a __message__ and a corresponding __Trace__, the `traceWith` function needs to be used:

```haskell
-- | Adds a message object to a trace
traceWith :: Trace m a -> a -> m ()

-- | An example call
traceWith trAddBlock (IgnoreBlockOlderThanK p)
```

This results in writing the message to different backends, depending on the configuration. The main backend is a __Forwarder__, which sends the message to cardano-tracer. Other backends are __Stdout__ for writing directly to the console and __EKGBackend__ for metrics.

Now we will explain all the library in more detail:

## Formatting

The `LogFormatting` typeclass is used to describe __trace presentation__ -- mapping
messages to __metrics__ and __text__.

* The `forMachine` method is used for a machine readable representation, which can be   varied through detail level.
  The implementation of this function is required for every message data type.

* the `forHuman` method shall represent the message in human readable form.
  It's default implementation defers to `forMachine`.

* the `asMetrics` method shall represent the message as `0` to `n` metrics.
  It's default implementation assumes no metrics. Each metric can optionally
  specify a namespace as a `[Text]`.

```haskell
class LogFormatting a where
  forMachine :: DetailLevel -> a -> A.Object

  forHuman :: a -> Text
  forHuman = forMachine DNormal

  asMetrics :: a -> [Metric]
  asMetrics v = []
```

### Detail level

An aspect of __trace presentation__ is the amount of details presented for each trace.  This is important, because the emitted __program traces__ might contain many details, which, if presented in full, would have made handling of the trace  expensive.  This detail control mechanism is configurable up to specific messages.

This detail level control is expressed by:

```haskell
data DetailLevel = DMinimal | DNormal | DDetailed | DMaximum
```

### Metrics

Metrics are provided by normal trace messages, which implement the `asMetrics` function of the `LogFormatting` typeclass. However, metrics are never filtered out.

As well metrics use their own names, which are different from the namespace of the messages.

`ekgTracer`is used as the metrics backend. It forwards the metrics to cardano-tracer for further processing.

```haskell
data Metric
  -- | An integer metric.
  -- Text is used to name the metric
    = IntM Text Integer
  -- | A double metric.
  -- Text is used to name the metric
    | DoubleM Text Double
  -- | A counter metric.
  -- Text is used to name the metric
    | CounterM Text (Maybe Int)
  deriving (Show, Eq)
```

## MetaTrace

The required meta info for the tracing system is provided by the typeclass __MetaTrace__.

```haskell
class MetaTrace a where
  namespaceFor  :: a -> Namespace a

  severityFor   :: Namespace a -> Maybe a -> Maybe SeverityS
  privacyFor    :: Namespace a -> Maybe a -> Maybe Privacy
  privacyFor _  _ =  Just Public
  detailsFor    :: Namespace a -> Maybe a -> Maybe DetailLevel
  detailsFor _  _ =  Just DNormal

  documentFor   :: Namespace a -> Maybe Text
  metricsDocFor :: Namespace a -> [(Text,Text)]
  metricsDocFor _ = []
  allNamespaces :: [Namespace a]
```

### Trace namespace

__Traces__ are organised into a hierarchical __tracer namespace__. (e.g. "ChainDB.OpenEvent.OpenedDB").
We base our implementation on the namespace, and require a one-to-one correspondence between namespaces and messages (bijective mapping). IMPORTANT: The namespaces must be given in a way, that globally all messages have a unique namespace.

We differentiate between two components of a namespace. The inner namespace, which is specified with the `namespaceFor` method, and the prefix namespace, which is given as argument to the `mkCardanoTracer` function. E.g in the namespace `ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB` the prefix is `ChainDB` and the inner part is `AddBlockEvent.IgnoreBlockAlreadyInVolatileDB`. In this way the same trace datatype can be used for different tracers with different prefixes(e.g. one time for `ChainSync.Local` and another time `ChainSync.Remote`).

```haskell
-- | A unique identifier for every message, composed of text
-- A namespace can as well appear with the tracer name (,
-- or more prefixes, in this moment it is a NamespaceOuter is used
data Namespace a = Namespace {
    nsPrefix :: [Text]
  , nsInner :: [Text]}
  deriving Eq
```

The __namespace__ is central to the functioning of trace-dispatcher and is used in different contexts:

* __trace-backend__, where the messages display the namespace.
* __documentation__, where it defines the overall structure of the generated documentation output,
* __configuration__, where it allows referring to tracer we want to reconfigure in some way, such as changing their severity,

ATTENTION: For every trace the function `allNamespaces` have to return a list of all namespaces. The correct implementation of this function is important for the correct working of trace-dispatcher.

### Severity

__Severity__ is expressed in terms of the enumeration provided by [section 6.2.1 of RFC 5424](https://tools.ietf.org/html/rfc5424#section-6.2.1):

```haskell
data SeverityS
    = Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency
```

..which ranges from minimum (`Debug`) to the maximum (`Emergency`) severity, and allows ignoring messages with severity level _below_ a configured global __severity cutoff__.

The severity of a message is specified by the `severityFor` method of class MetaTrace. It is mandatory to implement it for every trace datatype.

The severity is used for __Trace filtering__ together with a configuration severity. The trace is ignored, if the trace's __annotated severity__ is __less__ than its __configuration severity__.

### Privacy

__Privacy__ allows limiting __trace-outs__ that particular traces can reach. __Confidential__ privacy level means that the trace will not be externalised from the system, except via __standard output__.It is expressed in terms of:

```haskell
data Privacy
    = Confidential | Public
```

The privacy of a message is specified by the `privacyFor`method of class MetaTrace. It defaults to public if not specified.

Trace privacy cannot be configured.

__Trace filtering__ is affected by the __privacy context__ as follows:


1. `Confidential` traces can only reach the `stdout` __trace-out__.
2. `Public` traces reach both the `stdout` and `trace-forwarder` __trace-outs__.

In effect, it is impossible to leak the `Confidential` traces due to logging misconfiguration -- a leak can only happen if the user explicitly allows network access to the standard output of the traced program.

### Detail Level again

We already treated the detail level in the section about formatting, and the detail level for individual messages can be specified with the `detailsFor` function of class MetaTrace. The implementation is optional, and if not implemented the detail level defaults to `DNormal` for all messages.

If a value for the detail level is specified in the configuration, it is used instead of the detail level specified here!

### Documentation

The self-documentation features of `trace-dispatcher` use the text provided with the method `documentFor` to explain messages.

The method has to be implemented, but can return `Nothing` for messages, which have no documentation.

Documentation for metrics, are provided by the function `metricsDocFor`, which returns an array of tuples of text. The first text is the name of the metrics and the second is the provided documentation.

The implementation of this function is optional, and if not implemented it defaults to that the trace has no metrics.

ATTENTION: As the library uses this information internally as well for other purposes, it is mandatory to implement this function for all metrics. If no comment shall be given you can provide an empty text for the metrics name.

## Cardano tracer

To trace a message we need to construct a matching __Trace__ with the __mkCardanoTracer__ function.

```haskell
mkCardanoTracer :: forall msg.
     ( LogFormatting msg
     , MetaTrace msg)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> IO (Trace IO msg)

-- An example call look like this:
startupTr <- mkCardanoTracer trBase trForward mbTrEKG ["Startup"]
```

The trace gets as arguments the trace backends: `trStdout`, `trForward` and `mbTrEkg`, and a composed `name`, which is prepended to its namespace.
The backends are passed as arguments, as we need to only use one backend of any type for all tracers.

This function constructs a tracer with all the functionality described in this document.

## Configuration

Before the trace can be used we need to configure it with a configuration:

```haskell
-- | Configure this tracer with a configuration. The first argument is a state which needs to be passed, the second argument is the configuration and the last is the trace.
--
configureTracers :: forall a m.
     (MetaTrace a
  ,  MonadIO m)
  => ConfigReflection
  -> TraceConfig
  -> [Trace m a]
  -> m ()
```

The configuration is read from a file, but it has been decided to hard code a standard configuration for the purpose of ease of migration. This default configuration is given in the module [Cardano.Node.Tracing.DefaultTraceConfig](https://github.com/input-output-hk/cardano-node/blob/master/cardano-node/src/Cardano/Node/Tracing/DefaultTraceConfig.hs). In the config file all entries of the default configuration can be overridden. (To remove a frequency limiter, define a limiter with maxFrequency 0.0.)

The configuration file can be in JSON or YAML format. We will give the examples here in Yaml format

The namespaces are used for configuration values. Any value applies to all the trace messages which starts with the given namespace. To set a global value use the empty namespace "". This works down to individual messages, and the value of the more specific namespace overwrites the more general.

The options you can set for trace messages in this way are: Severity, Detail, Limiter and Backends.

### Configuring Severity

Specify a filter for the severity of the messages you want to see, e.g.:

```yaml
# Show messages of Severity Notice or higher as default
"":
    severity: Notice

  # But show ChainDB messages starting from Info
ChainDB:
    severity: Info
```

If you don't want to see any messages from tracers the new severity `Silence` exists, which suppresses all messages.

### Configuring Detail level

```yaml
"":
    # Keep this
    severity: Notice
    # All messages are shown with normal detail level
    detail: DNormal

Forge.Loop.AdoptedBlock:
    detail: DDetailed
```

Other options would be DMinimal and DMaximum. This has only an effect on messages which support the representation in different ways.

### Configuring Frequency limiting

Hardcoded eliding tracers are not supported in new-tracing, instead you can limit the frequency in which messages get shown.

```yaml
ChainDB.AddBlockEvent.AddedBlockToQueue:
    # Only show a maximum of 2 of these messages per second
    maxFrequency: 2.0
```

The activity of limiters will be written in the traces as well. It will write a StartLimiting message with the limiter name, when the limiter starts. It will write a RememberLimiting message every 10 seconds with the limiter name and the number of suppressed messages so far. Finally it will write a StopLimiting message with the limiter name and the total number of suppressed messages, when the limiter is deactivated through a lower number of arriving messages.

### Configuring Backends

Specify the backends the messages are routed to.

```yaml
"":
    # Keep this
    severity: Notice
    # And this
    detail: DNormal
    # And specify a list of backends to use
    backends:
      - Stdout MachineFormat
      - EKGBackend
      - Forwarder
```

These are all the backends currently supported. With Stdout you have the
options MachineFormat or HumanFormatColoured/HumanFormatUncoloured.
If messages don't support representation in HumanFormat* they are shown in MachineFormat anyway.

Forwarder means that messages are send to cardano-tracer

## Reconfiguration

The trace-dispatcher library allows tracer-reconfiguration at runtime without a
restart of the node. However, for a while both tracing systems will be present in parallel. In this transition time new tracing will for technical reason have a restricted functionality, so that the reconfiguration of a running node is currently not available.

## Special tracers

Some tracers are special, in that the trace system shall aggregate data, or do computations
on the data of messages. For these tracers you can use more basic functions of the underlying library, as we describe in this section.

If you want to change the general behavior of tracing, you may need to change the
implementation of the `mkCardanoTracer` function. But we will not treat this topic here.

### Hook

We provide a special version of mkCardanoTracer called mkCardanoTracer', which adds
as a last argument a hook functions, which allows it to transform the tracer in an `IO` context.

```haskell
-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall msg msg1.
     ( LogFormatting msg1
     , MetaTrace msg1)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> (Trace IO msg1 -> IO (Trace IO msg))
  -> IO (Trace IO msg)
```

As always in the world of contravariance, the hook function has to be read reversed,
so a trace of type msg is transformed to a type msg1. Of course you can use this hook
mechanism as well for transformations and aggregations which don't change the type, so that msg and msg1 are the same type.

### Fold-based aggregation

If aggregated information from multiple consecutive messages is needed the following fold functions can be used:


```haskell
-- | Folds the cata function with state acc over a.
-- Uses an MVar to store the state
foldMTraceM :: MonadUnliftIO m
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)

-- |The Folding helper type
newtype Folding a acc = Folding acc

-- |Function to unpack from Folding type
unfold :: Folding a acc -> acc
```

Since __tracers__ can be invoked from different threads, an `MVar` is used internally to secure correct behaviour.

As an example we want to log a measurement value together with the sum of all measurements that occurred so far.  For this we define a `Measure` type to hold a `Double`, a `Stats` type to hold the the sum together with the measurement and a `fold`-friendly function to calculate new `Stats` from old `Stats` and `Measure`:

```haskell
data Stats = Stats {
    sMeasure :: Double,
    sSum     :: Double
    }

calculateS :: Stats -> Double -> m Stats
calculateS Stats{..} val = pure $ Stats val (sSum + val)
```

Then we can define the aggregation  with the procedure foldMTraceM in the
following way, and it will output the Stats:

```haskell
  aggroTracer <- foldMTraceM calculateS (Stats 0.0 0.0) exTracer
```
### Trace routing

To send the message of a trace to different tracers depending on some criteria use the following function

-- | Allows to route to different tracers, based on the message being processed.
--   The second argument must mappend all possible traces of the first
--   argument to one . This is required for the configuration!

```haskell
routingTrace :: Monad m => (a -> m (Trace m a)) -> Trace m a -> m (Trace m a)
let resTrace = routingTrace routingf (tracer1 <> tracer2)
  where
    routingf LO1 {} = tracer1
    routingf LO2 {} = tracer2
```

The second argument must mappend all traces used in the routing trace function to one trace. This is required for the configuration. We could have construct a more secure interface by having a map of values to tracers, but the ability for full pattern matching outweigh this disadvantage in our view.
In the following example we send the messages of one trace to two tracers simultaneously:

```haskell
let resTrace = tracer1 <> tracer2
```

To route one trace to multiple tracers simultaneously we use the fact that Tracer is a `Semigroup` and then use `<>`, or `mconcat` for lists of tracers:

```haskell
(<>) :: Monoid m => m -> m -> m
mconcat :: Monoid m => [m] -> m
```

In the next example we unite two traces to one trace, for which we trivially use the same trace on the right side.

```haskell
tracer1  = appendName "tracer1" exTracer
tracer2  = appendName "tracer2" exTracer
```

## Documentation generation

The node can be called with the `trace-documentation` command, which takes the arguments
`config` and `output-file`, which are both path's to files. The first one points to a
valid configuration, and the second one depicts the generated output file.

```haskell
data TraceDocumentationCmd
  = TraceDocumentationCmd
    { tdcConfigFile :: FilePath
    , tdcOutput     :: FilePath
    }

runTraceDocumentationCmd
  :: TraceDocumentationCmd
  -> IO ()
```

A periodically generated documentation of the traces can be found in the cardano-node repository in the path `cardano-node/doc/new-tracing/tracers_doc_generated.md`

## DataPoints

DataPoints gives the ability for processes other then cardano-node to query the provided
runtime state of a node. DataPoints are equal to metrics, in that they are not written in textual
form to a log, but in contrast to metrics they have an ADT structure, so they can trace
any structured information. As a result, they give the ability for external processes
other then cardano-node to query the provided runtime state of a node (for example,
node's basic information).

DataPoints are implemented as special tracers, which packs the objects into DataPoint
constructors and require a ToJSON instance for that objects. The set of DataPoints
provided by the node is structured using the same namespace as metrics and log messages.
But otherwise DataPoints work independent of tracing, but are written in a local store,
so the latest value of a particular DataPoint can be queried on demand.

Also, [there is a document](https://github.com/input-output-hk/cardano-node/wiki/cardano-node-and-DataPoints:-demo)
describing how to accept DataPoints from an external process.

[`demo-acceptor`](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/demo/acceptor.hs)
application allows to ask for particular DataPoint by its name and display its value.

```haskell
-- A simple dataPointTracer which supports building a namespace
-- `trDataPoint`: shall be the datapoint backend
-- `namesFor`: shall be a function to produce a namespace for every
-- datapoint constructor
mkDataPointTracer :: forall dp. ToJSON dp
  => Trace IO DataPoint
  -> (dp -> [Text])
  -> IO (Trace IO dp)
mkDataPointTracer trDataPoint namesFor = do
    let tr = NT.contramap DataPoint trDataPoint
    pure $ withNamesAppended namesFor tr
```

## References

This is a document which is regenerated periodically and documents all trace-messages,  metrics and data-points in cardano-node. It as well displays the handling of these
messages with the current default configuration:

[Gernerated Cardano Trace Documentation](https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md)

For a quick start into new tracing see the document:

[New Tracing Quickstart](https://github.com/input-output-hk/cardano-node/blob/master/doc/New%20Tracing%20Quickstart.md)

This document describes a separate service for logging and monitoring Cardano nodes:

[Cardano Tracer](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md)

This document describes how to accept DataPoints from an external process:

[cardano node and DataPoints: demo](https://github.com/input-output-hk/cardano-node/wiki/cardano-node-and-DataPoints:-demo)





