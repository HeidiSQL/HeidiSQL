jHeidi Alpha 3.6 Release Notes
=====================================

fix more unquoted strings
fix more unquoted db and table names
fix unquoted db and field name
replace references to non worker initTree
fix non escaped db name
dont process unneeded events
add status tab to host info
parent message boxes correctly on multiple monitors systems
finally get rid of console scrolling bug
move waitoperation string into the worker
make data tab loader a worker
remove slow simulation
show loading messages
detect long running workers even on initial startup
handle refresh for workers properly
set wait message in worker
get wait message from worker
fix threading issues with the JIT compile
decrease class load times slightly
make tree init a worker
make host info script a worker
fix refresh
change refresh to use new worker stuff
only bail if the worker is done on longRunningTask, already running must always run
dont run detector if worker is cancelled immediately
ignore events we dont care about
change to new worker params
refactor workers to get rid of anon-inners and make workers smarter
make devmode only compile when necessary
set reaper as daemon thread
fix duplicate worker invocations
comment out slow motion simulation
fix refresh args
add better refresh handling
remove info logging
fix sql logging db
fix cache not flushing on refresh
fix classpath
sadly handle race condition
diff said there was a diff show checking in
get rid of synchronization and use only the first thread to watchdog the workers let others complete
fix longrunning task detector interrupts
fix NPE
fix NPEs and key the progress waiter so that only one runs per query
fix corruption due to using nonEDT thread
fix null pointer
make table tab data load asynch in case of long running op
handle case where worker is already running
add handling for lazy updates and cancelation
update lib with new status bar code
allow preload code for when a task is long running but not finished
Put all execution in try block so that stop button will be reset in case of unsucessful connection
get rid of ununsed condition
Add cut copy paste right click menus to editors
add popup support
various lib enhancements
add conditions to wireevents
dont flush the cache on refresh if the user is just refreshing the processes tab
fix bogus args passed to worker
fix problem with db not showing after host tab selected
fix overly generous perms
fix bogus worker cancellation
update ajl lib
add worker runoptions
add on cancel to kill statement
update ajl lib
add run button enable disable
add cancel handler
get rid of blank pixel
add ability to cancel JDBC statement on user queries not just the swing 
worker
fix conn cleanup
update ajl.jar
fix whitespace
make scripts actually work
add a key for workers so multiple workers of the same type can be 
launched without canceling a worker in progress for a different query
moved exe peer code to SystemUtil
use a real message loop instead of just handling WM_PAINT messages this finally gets rid of hourglass
src for mutex exe
update version
start message loop quicker
tab changes
only create mutex on first invocation
update ajl.jar
implement mutex for windows tasklist
copy mutex stub to dist dir
add bin dir to windows install
get rid of ExecWait since it puts an ugly hourglass up when launching jHeidi
add mutex stub app for windows tasklist
remove old exe
use unix zip to keep perms
add chmods for linux builds
update for alpha 3.5
update to v alpha 3.5
remove jps replace with tasklist
prepare for MySQL 5.1 bugs
Make nsis launcher parent to java.exe
latest autoupdater
remove compile warnings
change highlite color
fix search text on windows
fix compile errors
update ajl jar
dont use cache for some queries
fix script file name
fix refresh
make refresh worker aware
use non-cached data for dialogs
cache enhancements
remove unused script
flush show tables from cache on create
flush show databases from cache on create
exclude from caching
add ability to flush individual results
enable drop table and drop db on selection only
exclude user queries from caching
make refresh button flush DB cache
fix cache flush
turn on DB result cache
remove outdated comment
update ajl jar
remove unused file
make user queries asynch
fix handling of multiple overlapping tasks
fix startup state of toolbar buttons
update ajl jar 
add wires for query toolbar buttons
enable/disable table toolbar buttons
remove sysout
add support for component wiring
added wireevent support and DB changes
cleanup classes dir when building libs
get rid of clone use copy constructor
remove 3 second test delay
fix insets issue
update ajl jar
handle when the worker is cancelled vs done
make show table status asynch
add generic progress indicator to status bar
add better error handling
fix broken classpath
fix bogus timestamps
fix incorrect filename
add startEDT in the launch script
