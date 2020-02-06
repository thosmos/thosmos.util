#! /bin/bash
# update app version in pom.xml;
# `clojure -Spom` to update deps in pom;
clojure -A:jar build/thosmos.util.jar
mvn deploy:deploy-file -Dfile=./build/thosmos.util.jar -DpomFile=./pom.xml -Durl=https://clojars.org/repo -DrepositoryId=clojars
