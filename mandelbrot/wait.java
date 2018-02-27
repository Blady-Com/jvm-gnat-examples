
public class wait {
    
    //  This routine does nothing. It will return only when all JVM user
    //  threads  (except for the one that called this routine) have
    //  completed. This routine can be called by a thread that wnats to wait
    //  for all other *user* threads in the JVM to complete before proceeding.
    //  WARNING: if two user threads call this routine deadlock will occur as
    //  they will be waiting for each other to terminate before proceeding.
    
    public static void wait_user_thread_termination () {
	final int start_priority = Thread.currentThread().getPriority();
	
	// We do not want this routine to steal useful cycles from other user
	// threads so lower the priority and let other threads run.
	
	Thread.currentThread().setPriority (Thread.MIN_PRIORITY);
	Thread.yield();

	// Get the root thread group which allows us to ask the list of all
	// threads in the JVM.

	ThreadGroup root_group = Thread.currentThread().getThreadGroup();
	while (root_group.getParent() != null) {
	    root_group = root_group.getParent();
	}

	Thread [] threads = new Thread [50];
	while (true) {
	    // Make sure this routine does not run more often than once every
	    // 100 ms. If all user threads are at minimum pirity we do not
	    // want this routine to steal them cycles.

	    try {
		Thread.sleep (100);
	    }
	    catch (Exception e) {
		e.printStackTrace();
	    }

	    // If we have now more threads than the current size of array
	    // threads can accomodate increase the size of the array.
	    // Add some additional slots so that if the number of threads is
	    // increasing we do not have to reallocate the array at every
	    // loop iteration. This reduces the amount of garbage that the
	    // loop creates.

	    int nb_threads = root_group.activeCount();
	    if (threads.length < nb_threads) {
		threads = new Thread [nb_threads + 20];
	    }

	    boolean user_thread_found = false;
	    int count = root_group.enumerate (threads, true);

	    search : 
		for (int i = 0; i < count; i++) {
		    Thread t = threads [i];
		    if  (! t.isDaemon () && t != Thread.currentThread()) {
			user_thread_found = true;
			try {
			    t.join ();
			}
			catch (Exception e) {
			    e.printStackTrace();
			}
			break search;
		    }
		}

	    if (! user_thread_found) {
		Thread.currentThread().setPriority (start_priority);
		return;
	    }
	}
    }
}
