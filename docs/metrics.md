---
layout: page
title: Metrics
---

Colossus uses the (currently nameless) metrics library.

## Introduction

High-throughput Colossus services can serve hundreds of thousands of requests
per second, which can easily translate to millions of recordable events per
second.  The Metrics library provides a way to work with metrics with as little
overhead as possible.

The metrics library performs 3 important operations

* **Event Collection** : providing a way for user-code to signal that something worth tracking has happened
* **Metrics Aggregation** : collecting raw events into metrics that can be filtered and aggregated in real-time
* **Reporting** : packaging up and sending data to some external database (such as OpenTSDB)

## Basic Architecture

The heart of metrics is a `MetricSystem`, which is a set of actors that handle
all of the background operations of dealing with metrics.  In most cases, you
only want to have one `MetricSystem` per application.

Metrics are generated periodically by a `Tick` message published on the global
event bus.  By default this happens once per second, but it can be configured
to any time interval.  So while events are being collected as they occur,
compiled metrics (such as rates and histogram percentiles) are generated once
per tick.

### Structure of a Metric

Every metric in a system has a unique name with a url-like structure.  For
example, `/my-service/requests` and `/my-service/system/gc/msec` are two
metrics automatically generated by Colossus.  Every metric contains a set of
integer values, with each value uniquely identified by a set of key/value tags.
For example, the values for the `system/gc/msec` metric might look like

{% highlight scala %}

/my-service/system/gc/msec
  [type = ParNew] : 123456
  [type = ConcurrentMarkSweep] : 9876543


{% endhighlight %}

Tags are immensely useful when required more fine-grained breakdown of
imformation.  A metric value can have multiple tags, values in a single metric
do not all need the same tags, and tags are optional.  We will see later that
tags can also be used to filter and aggregate values.


## Getting Started

If you are using colossus, it depends on the metrics library and pulls it in.  Otherwise you must add the following to your build.sbt/Build.scala

{% highlight scala %}

libraryDependencies += "com.tumblr" %% "metrics" % "0.2.0"

{% endhighlight %}

### Quickstart

Here's a quick demo of using the metrics system.

{% highlight scala %}

import akka.actor._
import metrics._
import scala.concurrent.duration._

implicit val actor_system = ActorSystem()

//create the metric system
val metric_system = MetricSystem("/my-service")

//get a collection
val collection = metric_system.sharedCollection

//now get a rate from the collection
val rate = collection getOrAdd Rate("/my-rate", periods = List(1.second, 1.minute))

//fire off some events!
rate.hit()
rate.hit(25)

//arbitary maps of tags can be included with any event
rate.hit(tags = Map("key" -> "value"))

//by default, metrics are aggregated and snapshotted once per second
Thread.sleep(1000)

//the snapshot is sent to an akka agent in the metric_system
println(metric_system.snapshot()("/my-service/my-rate")())

{% endhighlight %}




## Event Collection

In most cases, metrics are generated from various events triggered by
application code.  For example, we can keep track of a service's request rate
by firing an event every time a request finishes processing.  Likewise, we can
keep track of latency by adding the proccessing time of each request to a
histogram.

Event Collectors take the role of providing a simple API for generating events.

There are currently 4 built-in event collectors:

* Counter - keeps track of a single number which can process increment/decrement operations
* Rate - A rate can be "hit" and it will track hits/second (or other time periods)
* Histogram - Similar to a rate, but each hit includes an integer value.  The histogram will generate percentiles/min/max over periods of time
* Gauge - set a static integer value

Each metric has numerous configuration options to ensure the best measurements for the job.


### Local vs Shared Collectors

There are two different ways to collect events.  Local event collectors run inside actors and are extremely efficient.  Shared collectors are meant to be used outside of an actor or shared amonng multiple actors, and every event becomes a single actor message.

In general it's best to use local collections whenever possible, since for
example incrementing a counter is just adding 1 to a `Long`, whereas with a
shared collection every increment is a single actor message.

To use a LocalCollection, the easiest method is to mixin the `ActorMetrics` trait to your actors.  For example:

{% highlight scala %}

class MyActor(val metricSystem: MetricSystem) extends Actor with ActorMetrics {

  //the trait automatically creates a LocalCollection called metrics  
  val rate = metrics getOrAdd Rate("/foos")

  def receive = handleMetrics orElse {
    case "Foo" => {
      rate.hit()
    }
    case "FOOOO" => Future { 
      rate.shared.hit() //thread-safe
    }
  }

}

{% endhighlight %}

### Using metrics within Colossus

Being an actor, a Colossus event loop has it's own `LocalCollection`.  This can be accessed through `context.worker.metrics`, for example:

{% highlight scala %}

Service.serve[Http]("my-service", 80) { context =>
  val myRate = context.worker.metrics.getOrAdd(Rate("my-rate"))
  context.handle{ connection =>
    connection.become{
      case request @ Get on Root / "hit" => {
        myRate.hit()
        request.ok("ok!")
      }
    }
  }
}

{% endhighlight %}

**Important** - in order to properly aggregate metrics together, workers add their id as a tag to every metric.



## Metric Snapshots and Aggregation

As mentioned before, the Metric system will generate a snapshot once per tick,
which defaults to once per second.  The easiest way to access this snapshot is
through the `snapshot` field on the `MetricSystem`.  This is an akka agent that
contains the most recent snapshot.

Notice that what values actually appear in the snapshot depend on how your
event collectors are configured.  For example, if you configure a rate to
aggregate in events per minute, the rate will only update its value once per
minute regardless of the frequency configured for snapshotting.  

## Metric Reporting

Currently metric reporting is mostly focused on reporting to OpenTSDB.  To setup reporting you basically need 2 things:

* A MetricSender - this is the object that encodes metrics to be sent
* A set of metric filters - These are used to select and aggregate which metrics to send

In addition to OpenTSDB, metrics may also be logged to file. To use logging, change the MetricSystem to use
a LoggingSender as the metrics reporter:

{% highlight scala %}

import akka.actor._
import metrics._
import scala.concurrent.duration._

implicit val actor_system = ActorSystem()

//create the metric system
val metric_system = MetricSystem("/my-service")

//create the config, providing LoggerSender as the MetricSender
val metric_config = MetricReporterConfig(LoggerSender)

//set this as the reporting for the metric system
metric_system.report(metric_config)

//get a collection
val collection = metric_system.sharedCollection

//proceed as normal

{% endhighlight %}

