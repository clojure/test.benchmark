Benchmark (and regression) suite for Clojure Copyright (c) Rich Hickey.

License and CA same as Clojure. See epl-v10.html at the root of the project for details.
## Getting Started
### Requirements
* JDK with java/javac on path - http://www.oracle.com/technetwork/java/javase/downloads/index.html
* maven - http://maven.apache.org/guides/getting-started/index.html

### Add the following to your ~/.m2/settings.xml file
    <settings>
    	<activeProfiles>
            <activeProfile>clojure-dev</activeProfile>
    	</activeProfiles>
    
    	<profiles>
            <profile>
                <id>clojure-dev</id>
    			<activation> <activeByDefault>false</activeByDefault> </activation>
                <repositories>
                  <repository>
                    <id>clojars</id>
                    <url>http://clojars.org/repo/</url>
                  </repository>
                </repositories>
            </profile>
            ...
        </profiles>
        ...
    </settings>

### Building
```bash
git clone git@github.com:clojure/test.benchmark.git
cd test.benchmark
mvn test
```

### Misc
```bash
# starts a swank server
mvn clojure:swank
```

### Running a benchmark (e.g. alioth.thread-ring)
```bash
script/run alioth.thread-ring 1000
```

### Timing a benchmark (e.g. alioth.fasta-redux)
```bash
time script/run alioth.fasta-redux 25000000 > /dev/null

real	0m2.389s
user	0m3.329s
sys	0m0.115s
```

In these results, alioth uses a measurement similar to the `real` timing.


### Running a baseline test
Baseline tests execute a java-equivalent benchmark and compare execution times to the respective clojure benchmarks.
The specifications for the baseline tests are in https://github.com/clojure/test.benchmark/blob/master/src/baseline/clojure/baseline/spec.clj

```bash
# this will pull down the non-CA repo into src/baseline and build
# Currently some Java programs require JDK 1.7 for Fork/Join library.
mvn compile -Pbaseline

# afterwards you can execute baseline tests as follows

# default, run all baseline tests 3x, sample and report
script/run baseline.exec

# run all baseline tests 10x, sample and report
script/run baseline.exec 10

# run mandelbrot and thread-ring baseline tests once and report
script/run baseline.exec 1 mandelbrot thread-ring
({:failures :none,
  :id "mandelbrot",
  :metrics {:runtime {:baseline 9451.773698, :target 10316.261803}},
  :tollerances {:runtime 0.25},
  :sample-size 1}
 {:failures :none,
  :id "thread-ring",
  :metrics {:runtime {:baseline 4308.804918, :target 309.521524}},
  :tollerances {:runtime -9.0},
  :sample-size 1})
```

TODO: hook baseline test execution into maven test cycle and generate a jenkins-consumable report

## Contributing
There are a number of completed benchmarks, but there is a lot more left to do.
Andy Fingerhut has a suit of benchmarks tailored for Clojure 1.2 and need updated or redone for Clojure 1.3 (https://github.com/jafingerhut/clojure-benchmarks).

A good general approach is to examine the fastest alioth implementations, usually Java or C, and write a Clojure port.
Fast small programs in any language are also worthy of examination. These benchmarks should demonstrate how to write idiomatic high-performance Clojure.

Long term goal is a performance regression test suit ...

Alioth Benchmark site is here: http://shootout.alioth.debian.org/

Work on the benchmarks is tracked here: http://dev.clojure.org/jira/browse/TBENCH
