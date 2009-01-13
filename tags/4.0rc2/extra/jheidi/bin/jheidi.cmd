@echo off

java -DstartEDT=true -DusePrecompiledClassFiles=true -cp jheidi.jar;lib/ajl.jar;lib/swing-worker-1.1.jar;lib/editor.jar;lib/mysql-connector-java-5.0.7-bin.jar;lib/ojdbc14.jar;lib/log4j-1.2.13.jar -Dlog4jconfig=logs/log4jconfig.xml com.rendion.ajl.AjlScript jheidi
