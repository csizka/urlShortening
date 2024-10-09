FROM sbtscala/scala-sbt:eclipse-temurin-alpine-22_36_1.10.2_3.5.0

ADD src src
ADD build.sbt build.sbt
ADD project/build.properties project/build.properties
ADD project/plugins.sbt project/plugins.sbt

RUN ["sbt", "assembly"]

ENTRYPOINT ["java", "-jar", "target/scala-3.3.3/url-shortening.jar"]