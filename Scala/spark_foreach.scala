val conf = new SparkConf().setMaster("local[4]").setAppName("NetworkWordCount")

// (...)
 
 val counter = new AtomicInteger(0)
 
 processedStream.foreachRDD { (rdd, time) =>
      output("Enter in foreach rdd: " + counter.get())
      rdd.foreachPartition{partition =>
        output("Enter in foreach partition: " + counter.get())
        partition.foreach {
          record =>
            output("Try to write to kafka: " + counter.get())
            /*write to kafka*/
            Thread.sleep(10 * 1000)
            output("after kafka trial: " + counter.get())
            throw new Exception("no kafka is configured! " + record)
        }}

      output("After Kafka: " + counter.incrementAndGet())
    }
    
// (...)

/*
=======================  LOG  =======================
streaming-job-executor-0: Enter in foreach rdd: 0
Executor task launch worker for task 1: Enter in foreach partition: 0
streaming-job-executor-0: After Kafka: 1
streaming-job-executor-0: Enter in foreach rdd: 1
Executor task launch worker for task 21: Enter in foreach partition: 1
Executor task launch worker for task 21: Try to write to kafka: 1
Executor task launch worker for task 21: after kafka trial: 1
streaming-job-executor-0: Enter in foreach rdd: 1
/*
