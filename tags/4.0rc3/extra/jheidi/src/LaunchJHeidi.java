public class LaunchJHeidi
{
	public static void main(String[] args) 
	{
		System.setProperty("usePrecompiledClassFiles", "true");
    System.setProperty("startEDT", "true");
		System.setProperty("log4jconfig", "logs/log4jconfig.xml");
		com.rendion.ajl.AjlScript.main(new String[] {"jheidi"});
	}
}
