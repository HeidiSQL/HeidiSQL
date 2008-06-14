
This readme applies to the src distro of jHeidi ONLY.
If you are not modifying the app pls download the binary distro.

Define JAVA_HOME to point to the root of your JDK installation.

It is not legal to ship tools.jar separately from the JDK so setting JAVA_HOME allows the 
script compiler to work.

Once this is done, you may make modifications to scripts in the root dir named *.ajl
restart the application and your changes will be seen.

run the application using the following command.  

ajl jheidi.

There is an option to use an automatic class reloader so you dont have to restart the application
to see your changes however it is not guaranteed to work in this alpha release 
as it has not been fully tested with this application.  The next release should support it.

Java 1.6 is recommended but Java 1.5 should work.  Please dont develop on old JDKs.

This application and all associated non-third party code is in the public domain
and copyright laws are not applicable.  


