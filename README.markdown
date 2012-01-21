## Getting Started
### Requirements
* JDK with java/javac on path - http://www.oracle.com/technetwork/java/javase/downloads/index.html
* maven - http://maven.apache.org/guides/getting-started/index.html
* add the following to your ~/.m2/settings.xml file
```
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
```
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
## Contributing
There are a number of completed benchmarks, but there is a lot more left to do.
Andy Fingerhut has a suit of benchmarks tailored for Clojure 1.2 and need updated or redone for Clojure 1.3 (https://github.com/jafingerhut/clojure-benchmarks).

A good general approach is to examine the fastest alioth implementations, usually Java or C, and write a Clojure port.
Idiomatic Clojure typically shows poorly in the benchmarks. Liberal use of primitive arrays, type hinting, and iteration apply.

Long term goal is a performance regression test suit ...

Alioth Benchmark site is here: http://shootout.alioth.debian.org/

Work on the benchmarks is tracked here: http://dev.clojure.org/jira/browse/TBENCH
