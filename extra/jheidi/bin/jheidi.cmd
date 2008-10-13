@echo off

java -DstartEDT=true -DusePrecompiledClassFiles=true -cp jheidi.jar;lib/ajl.jar;lib/editor.jar;lib/mysql-connector-java-5.0.7-bin.jar;lib/log4j-1.2.13.jar -Dlog4jconfig=logs/log4jconfig.xml com.rendion.ajl.AjlScript jheidi
