# trace-dispatcher: efficient, simple and flexible program tracing

`trace-dispatcher` is a simple, efficient __tracing library__, that is based on the contravariant tracing library and should use little resources in the executed program, has a pleasant API, and provides self-documenting features.

`trace-dispatcher` consists of generic combinators, which are then used in functions specific for the cardano application.

## Contents

1. [Contents](#contents)
2. [Overview](#overview)
3. [Formatting](#formatting)
    1. [Detail level](#detail-level)
    2. [Metrics](#metrics)
4. [MetaTrace](#metatrace)
    1. [Trace namespace](#trace-namespace)
    2. [Severity](#severity)
    3. [Privacy](#privacy)
    4. [Detail level again](#detail-level-again)
    5. [Documentation](#documentation)
5. [Cardano tracer](#cardano-tracer)
6. [Configuration](#configuration)

## Overview

In this overview we shortly touch a lot of aspects, which will be explained in more detail later.

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

An aspect of __trace presentation__ is the amount of details presented for each trace.  This is important, because the emitted __program traces__ might contain extreme details, which, if presented in full, would have made handling of the trace extremely expensive.  This detail control mechanism is configurable up to specific messages.

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

__Traces__ are organised into a hierarchical __tracer namespace__. (e.g. "ChainDB.OpenEvent.OpenedDB"). We repeat that the namespaces must be given in a way, that globally all messages have a unique namespace.

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

If a value for the detail level is specified in the configuration, it is usd instead of the detail level specified here!

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






The configurability of __dispatchers__ this library allows to define is based on:

1. __Tracer namespace__-based configurability, down to single __message__ granularity,
2. Runtime reconfigurability, triggered by invocation of `configureTracers`,
3. Documentation entries __message__.

Reconfiguration can be triggered at runtime and essentially involves running the entire __dispatcher__ trace network, by doing trace specialisation for each trace that has documentation entries defined.

```haskell
-- The function configures the traces with the given configuration
configureTracers :: Monad m => TraceConfig -> Documented a -> [Trace m a]-> m ()
```

These are the options that can be configured based on a namespace:

```haskell
data ConfigOption =
    -- | Severity level for filtering (default is Warning)
    ConfSeverity SeverityF
    -- | Detail level of message representation (Default is DNormal)
  | ConfDetail DetailLevel
  -- | To which backend to pass
  -- Default is [EKGBackend, Forwarder, Stdout HumanFormatColoured]
  | ConfBackend [BackendConfig]
  -- | Construct a limiter with name (Text) and limiting to the Double,
  -- which represents frequency in number of messages per second
  | ConfLimiter Double

data BackendConfig =
    Forwarder
  | Stdout FormatLogging
  | EKGBackend

data TraceConfig = TraceConfig {
     -- | Options specific to a certain namespace
    tcOptions            :: Map.Map NamespaceOuter [ConfigOption]
     -- | Options for trace-forwarder
  , ...
}
```

If the configuration file is in Yaml format, the following entry means, that by default
all messages with Info or higher Priority are shown:

```yaml
```

But if you want to see Debug messages of the ChainDB trace, then add:

```yaml
  Node:
    severity: Info
  Node.ChainDB:
    severity: Debug
```

And if you never want to see any message of the AcceptPolicy trace, then add:

```yaml
  Node:
    severity: Info
  Node.ChainDB:
    severity: Debug
  Node.AcceptPolicy:
    severity: SilentF
```

As another example, if you don't want to see more then 1 BlockFetchClient
message per second, then add this to your configuration file:

```yaml
  Node.BlockFetchClient:
    maxFrequency: 1.0
```

In Cardano a default configuration is given in the module [Cardano.Node.Tracing.DefaultTraceConfig](https://github.com/input-output-hk/cardano-node/blob/master/cardano-node/src/Cardano/Node/Tracing/DefaultTraceConfig.hs). In the config file all entries of the default configuration can be overridden. To remove a frequency limiter, define a limiter with maxFrequency 0.0.





