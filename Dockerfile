FROM sbtscala/scala-sbt:eclipse-temurin-alpine-22_36_1.10.2_3.5.0

ADD build.sbt build.sbt
ADD project/build.properties project/build.properties
ADD project/plugins.sbt project/plugins.sbt
RUN ["sbt", "update"]

ADD src/main src/main
RUN ["sbt", "assembly"]

ENTRYPOINT ["java", "-jar", "target/scala-3.5.0/url-shortening.jar"]