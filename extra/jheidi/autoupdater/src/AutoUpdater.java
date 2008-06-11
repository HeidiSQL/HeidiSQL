package com.rendion.ajl;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import java.util.List;
import javax.swing.*;

import org.jdesktop.swingworker.SwingWorker;

public class AutoUpdater implements Serializable
{
  public static String PROP_PATH = ".autoupdater";
  public static String NEW_PROP_PATH = "new.autoupdater";
  public static String UPDATE_OPTIONS_FILE = ".update-options";
  public static String UPDATE_PROPS_FILE = ".update-props";
  private static String PS_CMD = "ps aux";
  private static String JPS_CMD = "jps -m";
  private static String WIN_KILL = "taskkill";
  private static String KILL = "kill";
  private boolean onWindows = System.getProperty("os.name").toUpperCase().indexOf("WINDOWS") != -1;
  private boolean onMac = System.getProperty("os.name").toUpperCase().startsWith("MAC");
  private boolean updatedAppPropsSent = false;
  private String autoUpdateJar;

  public AppProps appProps;
  public UpdateProps updateProps;
  public UpdateOptions updateOptions;

  enum Source
  {
    WEB, FILE
  };

  public AutoUpdater()
  {
    appProps = new AppProps();
    autoUpdateJar = appProps.libPath + "/" + appProps.jar;
    
    System.out.println(appProps);
  }

  public void postInstallCleanup()
  {
     /*
     * if this is loaded at app startup its a good place to
     * cleanup the downloaded autoupdater.jar since the jar will 
     * be locked while the actual update runs
     */
    
    deleteFile(autoUpdateJar);
  }
  
  public boolean updateExists()
  {
    updateProps = new UpdateProps(Source.WEB);
    autoUpdateJar = appProps.libPath + "/" + updateProps.jar;
    
    System.out.println(updateProps);
    return updateProps.timestamp.compareTo(appProps.timestamp) > 0;
  }

  public void update(boolean showUI, boolean restart)
  {
    updateOptions = new UpdateOptions(showUI, restart);
    
    deleteFile(autoUpdateJar);
    deleteFile(NEW_PROP_PATH);
    updateProps.delete();
    updateOptions.delete();

    urlToFile(updateProps.urlRoot + "/" + updateProps.jar, autoUpdateJar);
    updateProps.save();
    updateOptions.save();

    Runtime rt = Runtime.getRuntime();
    try
    {
      rt.exec("java -jar " + autoUpdateJar);
    }
    catch (Exception e)
    {
      try  {  deleteFile(autoUpdateJar);  } catch (Exception e1) {}
      try  {  deleteFile(NEW_PROP_PATH);  } catch (Exception e1) {}
      try  {  updateOptions.delete(); } catch (Exception e2) {}
      try  {  updateProps.delete(); } catch (Exception e3) {}

      throw new RuntimeException("Update failed!", e);
    }

    System.exit(0);
  }

  public static void main(String[] args)
  {
    AutoUpdater updater = new AutoUpdater();
    updater.runUpdate();
  }
  
  private void shutdownAllInstances()
  {
    if ( onMac )
      return;

    Object[] pids = getPIDsOfOpenInstances();

    if ( pids == null || pids.length == 0 )
      return;

    StringBuilder buffer = new StringBuilder(100);

    buffer.append(onWindows ? WIN_KILL : KILL);

    for (Object pid : pids)
    {
      if ( onWindows )
      {
        buffer.append(" /PID");
      }
      buffer.append(" ").append(pid);
    }
    
    attachTo(buffer.toString(), new String[] {}, false, null);

  }

  private void runUpdate()
  {
    updateProps = new UpdateProps(Source.FILE);
    updateOptions = new UpdateOptions();
    
    shutdownAllInstances();

    if(!updateFiles())
    {
      JOptionPane.showMessageDialog(null, appProps.version + " install cancelled.", "Auto Update Cancelled", JOptionPane.ERROR_MESSAGE);
      return;
    }
    
    execAfterDownload();
    
    doFileCleanup();
    
    commitNewVersion();
    
    boolean doRestart = updateOptions.restart && appProps.restartCommand != null;
    
    if (updateProps.execAfterDownload == null)
    {
      String msg = appProps.app + " " + appProps.version + " install complete!";
      
      if (!doRestart)
      {
        msg+="\n\nPlease restart " + appProps.app + ".";
      }
      
      JOptionPane.showMessageDialog(null, msg, "Auto Update Complete", JOptionPane.INFORMATION_MESSAGE);
    }

    if (doRestart)
    {
      restartApp();
    }

    System.exit(0);
	}
	
	private void execAfterDownload()
  {
    if (updateProps.execAfterDownload != null)
    {
      //split and execute and wait
      
    }
    
  }

  private void doFileCleanup()
  {
    updateProps.delete();
    updateOptions.delete();
    
    if (updateProps.cleanupFiles != null)
    {
      //split and delete
    }

  }

  private void commitNewVersion()
  {
    if ( updatedAppPropsSent )
    {
      appProps.copyNewVersion();
    }
    else
    {
      appProps.update(updateProps);
    }

  }

  private boolean updateFiles()
  {
    if ( updateOptions.showUI )
    {
      
      return updateFilesWithProgress();
    }
    
    String[] urls = updateProps.urls.split(":");
    for (String url : urls)
    {
      if ( NEW_PROP_PATH.equals(url) )
      {
        updatedAppPropsSent = true;
      }
      urlToFile(updateProps.urlRoot + "/" + url, url);
    }
    return true;

  }

  private boolean updateFilesWithProgress()
  {
    if (updateProps.urlLengths == null || updateProps.urlLengths.equals(""))
    {
      throw new RuntimeException("Cant show progress bar without urlLengths specified in updateprops");
    }
        
    int totalLength = getTotalDownloadLength();
        
    ProgressBar progress = new ProgressBar(totalLength) {
        protected Object doInBackground() {
      
          String[] urls = updateProps.urls.split(":");
          for (String url : urls)
          {
            if ( NEW_PROP_PATH.equals(url) )
            {
              updatedAppPropsSent = true;
            }
            urlToFile(updateProps.urlRoot + "/" + url, url, this);
          }
          try {Thread.sleep(500);} catch(Exception e) {}
          this.frame.dispose();
          return null;
        }
    };
    
    progress.show();
    
    while(!progress.isDone() && !progress.isCancelled()) {}
    
    return !progress.isCancelled();
    
  }

  private int getTotalDownloadLength()
  {
    String[] fileLengths = updateProps.urlLengths.split(":");
    
    int totalLength = 0;
    for (String len: fileLengths)
    {
      totalLength+=Integer.valueOf(len);
    }
    return totalLength;
  }

  private void restartApp()
  {
    Runtime rt = Runtime.getRuntime();
    try
    {
      rt.exec(appProps.restartCommand);
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }

  }

  public Object[] getPIDsOfOpenInstances()
  {
    AUStreamOutput out = new AUStreamOutput();

    attachTo(onWindows ? JPS_CMD : PS_CMD, new String[] {}, false, out);
    int waits = 0;

    while ((out.getOutput().length() == 0 || !out.closed()) && waits < 200)
    {
      try { Thread.sleep(100); } catch (Exception e) { waits++; }
    }

    return parseOutput(out.getOutput());

  }

  private static void populate(Object o, Properties p)
  {
    for (Object key : p.keySet())
    {
      try
      {
        Field f = o.getClass().getDeclaredField((String) key);
        if ( f.getType().equals(boolean.class) )
        {
          f.set(o, Boolean.valueOf(p.getProperty((String) key)));
        }
        else
        {
          f.set(o, p.getProperty((String) key));
        }
      }
      catch (Exception e)
      {
        throw new RuntimeException("Cant populate " + o.getClass().getName() + " from properties", e);
      }

    }

  }

  private static String toString(Object o)
  {
    String s = "";
    try
    {
      Field[] fields = o.getClass().getDeclaredFields();
      for (Field f : fields)
      {
        if ( !f.getName().startsWith("this$") )
        {
          if ( f.get(o) != null )
          {
            s += f.getName() + "=" + f.get(o) + "\n";
          }
        }
      }
    }
    catch (Exception e)
    {
      throw new RuntimeException("Cant output " + o.getClass().getName() + " properties", e);
    }

    return s;

  }

  private String getText(String urlAsString)
  {
    String response = "";
    int bytes;
    char[] buf = new char[10000];
    HttpURLConnection con = null;

    try
    {
      URL url = new URL(urlAsString);
      con = (HttpURLConnection) url.openConnection();

      int result = con.getResponseCode();
      if ( result == HttpURLConnection.HTTP_OK )
      {
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        while (true)
        {
          bytes = in.read(buf);
          if ( bytes == -1 )
          {
            break;
          }
          else
          {
            response += new String(buf, 0, bytes);
          }
        }
      }
      else
      {
        throw new RuntimeException("HTTP Response Code: " + result);
      }
    }
    catch (Exception e)
    {
      if ( !(e instanceof RuntimeException) )
      {
        throw new RuntimeException("Cant open autoupdate file", e);
      }
      else
        throw (RuntimeException) e;
    }
    finally
    {
      try { con.disconnect(); } catch (Throwable t) {}
    }

    return response;

  }
  
  private void urlToFile(String urlAsString, String localPath)
  {
    urlToFile(urlAsString, localPath, null);
  }

  private void urlToFile(String urlAsString, String localPath, ProgressBar progress)
  {
    FileOutputStream f = null;
    URLConnection con = null;
    InputStream is = null;
    BufferedOutputStream out = null;

    if ( progress != null )
    {
      progress.publish(localPath);
    }
    
    try
    {
      URL url = new URL(urlAsString);
      con = url.openConnection();
      is = con.getInputStream();
      BufferedInputStream in = new BufferedInputStream(is);
      f = new FileOutputStream(localPath);
      out = new BufferedOutputStream(f);
      int i;
      while ((i = in.read()) != -1)
      {
        out.write(i);
        if ( progress != null )
        {
          progress.totalDone++;
          progress.progress();
        }
      }
      out.flush();
    }
    catch (Exception e)
    {
      e.printStackTrace();
      throw new RuntimeException("AutoUpdater file unavailable:" + localPath, e);
    }

    finally
    {
      try { is.close(); } catch (Throwable t) { }
      try { out.close(); } catch (Throwable t) { }
      try { f.close(); } catch (Throwable t) { }
    }
  }

  private static void deleteFile(String path)
  {
    File f = new File(path);
    if ( !f.exists() )
    {
      return;
    }
    f.delete();
    if ( f.exists() )
    {
      throw new RuntimeException(path + " could not be deleted!");
    }

  }

  public static final void save(String path, Object o)
  {
    BufferedWriter out = null;
    String s = o.toString();

    try
    {
      out = new BufferedWriter(new FileWriter(path));
      out.write(s, 0, s.length());
    }
    catch (Exception e)
    {
      throw new RuntimeException("Cannot save file:" + path);
    }
    finally
    {
      try { out.close(); } catch (Exception e) {}
    }

  }

  public static void copy(String srcPath, String destPath)
  {
    copy(new File(srcPath), new File(destPath));

  }

  public static void copy(File src, File dst)
  {
    InputStream in = null;
    OutputStream out = null;
    try
    {
      in = new FileInputStream(src);
      out = new FileOutputStream(dst);
      byte[] buf = new byte[1024];
      int len;
      while ((len = in.read(buf)) > 0)
      {
        out.write(buf, 0, len);
      }
    }
    catch (IOException e)
    {
      throw new RuntimeException(e);
    }
    finally
    {
      try { in.close(); } catch (Exception e) {}
      try { out.close(); } catch (Exception e) {}
    }
  }

  public static void rename(String path, String to)
  {
    File f = new File(path);
    File t = new File(to);
    if ( !f.exists() )
    {
      throw new RuntimeException("The path does not exist to be renamed.");
    }
    if ( t.exists() )
    {
      throw new RuntimeException("Cannot rename.  The path already exists.");
    }
    f.renameTo(t);
    if ( !t.exists() )
    {
      throw new RuntimeException("The path could not be renamed or moved.");
    }
  }

  public Object[] parseOutput(String output)
  {
    List lines = new ArrayList();
    int index = output.indexOf(appProps.processKey);
    int lastIndex = 0;
    while (index != -1)
    {
      int prevLineIndex = output.substring(lastIndex, index).lastIndexOf("\n");
      String line = output.substring(prevLineIndex + 1, index);
      lines.add(line);

      System.out.println(line);

      index = output.indexOf(appProps.processKey, index + 1);
    }

    for (int i = 0; i < lines.size(); i++)
    {
      lines.set(i, parsePIDFromPSLine((String) lines.get(i), onWindows ? 0 : 1));
    }

    return lines.toArray();
  }

  private String parsePIDFromPSLine(String line, int col)
  {
    int curCol = 0;
    char lastChar = 0;
    StringBuilder value = new StringBuilder(10);
    for (int i = 0; i < line.length(); i++)
    {
      char curChar = line.charAt(i);
      if ( curChar == ' ' && lastChar != ' ' )
      {
        curCol++;
        if ( curCol > col )
          break;
      }
      if ( curCol == col && curChar != ' ' )
      {
        value.append(line.charAt(i));
      }
      lastChar = curChar;

    }

    return value.toString();

  }

  public int attachTo(String command, String[] args, boolean invokeShell, AUStreamOutput output)
  {
    int returnValue = -1;
    Process proc = null;
    String[] cmd = null;
    try
    {

      if ( !invokeShell )
      {
        cmd = new String[args.length + 1];
        cmd[0] = command;
        System.arraycopy(args, 0, cmd, 1, args.length);
      }
      else
      {
        cmd = new String[args.length + 3];
        cmd[0] = "cmd.exe";
        cmd[1] = "/C";
        cmd[2] = command;
        System.arraycopy(args, 0, cmd, 3, args.length);
      }

      Runtime rt = Runtime.getRuntime();
      if ( args.length > 0 )
      {
        proc = rt.exec(cmd);
      }
      else
      {
        // proc = rt.exec(invokeShell? "cmd.exe /C " + command : command);
        // proc = rt.exec(invokeShell? "bash " + command : command);
        proc = rt.exec(command);
      }

      AUStreamGobbler errorGobbler = new AUStreamGobbler(proc.getErrorStream(), "ERROR", output);

      AUStreamGobbler outputGobbler = new AUStreamGobbler(proc.getInputStream(), "OUTPUT", output);

      errorGobbler.start();
      outputGobbler.start();
      returnValue = proc.waitFor();
      if ( output != null )
        output.close();

    }
    catch (Throwable t)
    {
      System.out.println(t);
      throw new RuntimeException("Could not execute command " + command, t);
    }

    return returnValue;
  }

  public class UpdateOptions
  {
    public boolean showUI;
    public boolean restart;

    public UpdateOptions(boolean showUI, boolean restart)
    {
      this.showUI = showUI;
      this.restart = restart;
    }

    public UpdateOptions()
    {
      Properties p = new Properties();
      FileInputStream in = null;
      try
      {
        in = new FileInputStream(UPDATE_OPTIONS_FILE);
        p.load(in);
      }
      catch (Exception e)
      {
        throw new RuntimeException("Cant load AutoUpdater:updateOptions!", e);
      }
      finally
      {
        try { in.close(); } catch (Exception e) {}
      }
      populate(this, p);
    }

    public void save()
    {
      AutoUpdater.save(UPDATE_OPTIONS_FILE, this);
    }

    public void delete()
    {
      deleteFile(UPDATE_OPTIONS_FILE);
    }

    public String toString()
    {
      return AutoUpdater.toString(this);
    }

  }

  public class AppProps
  {
    public String app;
    public String version;
    public String timestamp;
    public String url;
    public String restartCommand;
    public String processKey;
    public String libPath;
    public String propPath;
    public String releaseNotes;
    public String jar = "autoupdater.jar";

    AppProps()
    {
      Properties p = new Properties();
      FileInputStream in = null;
      try
      {
        in = new FileInputStream(PROP_PATH);
        p.load(in);
      }
      catch (Exception e)
      {
        throw new RuntimeException("Cant load AutoUpdater:AppProps!", e);
      }
      finally
      {
        try { in.close(); } catch (Exception e) {}
      }
      populate(this, p);

    }

    public void update(UpdateProps updateProps)
    {
      version = updateProps.version;
      timestamp = updateProps.timestamp;
      jar = updateProps.jar;
      if ( updateProps.releaseNotes != null )
      {
        releaseNotes = updateProps.releaseNotes;
        releaseNotes = releaseNotes.replaceAll("\r", "");
        releaseNotes = releaseNotes.replaceAll("\n", "\\\\n");
      }
      save();
    }

    public void copyNewVersion()
    {
      AutoUpdater.deleteFile(PROP_PATH + ".bak");
      AutoUpdater.rename(PROP_PATH, PROP_PATH + ".bak");
      AutoUpdater.rename(NEW_PROP_PATH, PROP_PATH);

    }

    public void save()
    {
      AutoUpdater.save(PROP_PATH, this);
    }

    public String toString()
    {
      return AutoUpdater.toString(this);
    }

  }

  public class UpdateProps
  {
    public String version;
    public String timestamp;
    public String urlRoot;
    public String jar;
    public String execAfterDownload;
    public String cleanupFiles;
    public String urls;
    public String urlLengths;
    public String releaseNotesUrl;
    public String releaseNotes;
    

    UpdateProps(Source source)
    {
      Properties p = new Properties();
      InputStream in = null;
      try
      {

        if ( source == Source.WEB )
        {
          String propString = getText(appProps.url);
          in = new ByteArrayInputStream(propString.getBytes());
        }
        else if ( source == Source.FILE )
        {
          in = new FileInputStream(UPDATE_PROPS_FILE);
        }
        p.load(in);
        
      }
      catch (Exception e)
      {
        throw new RuntimeException("Cant load AutoUpdater:UpdateProps!", e);
      }
      finally
      {
        try {in.close(); } catch (Exception e) { }
      }
      populate(this, p);
      
      if ( source == Source.WEB && releaseNotesUrl != null)
      {
        releaseNotes = getText(urlRoot + "/" + releaseNotesUrl);
      }

    }

    public String toString()
    {
      return AutoUpdater.toString(this);
    }

    public void save()
    {
      releaseNotes = releaseNotes.replaceAll("\r", "");
      releaseNotes = releaseNotes.replaceAll("\n", "\\\\n");
      AutoUpdater.save(UPDATE_PROPS_FILE, this);

    }

    public void delete()
    {
      deleteFile(UPDATE_PROPS_FILE);
    }

  }

  public class AUStreamGobbler extends Thread
  {

    InputStream is;
    String type;
    AUStreamOutput output;

    AUStreamGobbler(InputStream is, String type)
    {
      this.is = is;
      this.type = type;
    }

    AUStreamGobbler(InputStream is, String type, AUStreamOutput output)
    {
      this.is = is;
      this.type = type;
      this.output = output;
    }

    public void run()
    {
      try
      {
        InputStreamReader isr = new InputStreamReader(is);
        BufferedReader br = new BufferedReader(isr);
        String line = null;
        while ((line = br.readLine()) != null)
          if ( output != null )
          {
            output.setOutput(line + "\n");
          }
          else
          {
            System.out.println(type + ">" + line);
          }
      }
      catch (IOException ioe)
      {
        ioe.printStackTrace();
      }
    }
  }

  public class AUStreamOutput
  {
    private String output = "";
    private boolean updated = false;
    private boolean closed = false;

    public synchronized void setOutput(String output)
    {
      this.output += output;
      updated = true;
    }

    public boolean closed()
    {
      return this.closed;
    }

    public boolean updated()
    {
      return this.updated;
    }

    public synchronized void close()
    {
      this.closed = true;

    }

    public String getOutput()
    {
      return output;

    }

  }
  
  public abstract class ProgressBar extends SwingWorker implements PropertyChangeListener, ActionListener
  {
    public int totalWork = 0;
    public JProgressBar barWidget;
    public JLabel currentFile;
    public JLabel statusLabel;
    public JFrame frame;
    public int totalDone = 0;
    public ProgressBar(int totalWork)
    {
      this.totalWork = totalWork;
      this.addPropertyChangeListener(this);
    }
    
    private void setLAndF()
    {
      try
      {
        JFrame.setDefaultLookAndFeelDecorated(true);
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception e)
      {
        System.out.println("Unable to load native look and feel");
      }
    }
      
    
    public void show()
    {
      setLAndF();
      javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                frame = new JFrame(updateProps.version);
                JPanel panel = (JPanel) frame.getContentPane();
                panel.setLayout(null);
                frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

                barWidget = new JProgressBar(0, 100);
                
                barWidget.setSize(350, 20);
                barWidget.setLocation(10, 35);
                barWidget.setValue(0);
                
                JLabel label = new JLabel();
                label.setFont(label.getFont().deriveFont(Font.BOLD));
                label.setText("Downloading: ");
                label.setSize(100, 15);
                label.setLocation(10, 10);
                currentFile =  new JLabel();
                currentFile.setLocation(110, 10);
                currentFile.setSize(200, 15);
                
                JButton button = new JButton("Cancel");
                button.setSize(110, 25);
                button.setLocation(127, 76);
                button.addActionListener(ProgressBar.this);
                
                statusLabel = new JLabel("0/" + totalWork, JLabel.RIGHT);
                statusLabel.setSize(210, 15);
                statusLabel.setLocation(150, 60);
                
                panel.add(label);
                panel.add(barWidget);
                panel.add(currentFile);
                panel.add(button);
                panel.add(statusLabel);

                
                frame.setSize(378, 133);
                frame.setLocationRelativeTo(null);
                frame.setResizable(false);
                frame.setVisible(true);
                execute();

            }
        });
    }
    
    protected void process(List arg0)
    {
      if (!isCancelled())
      {
        currentFile.setText((String)arg0.get(arg0.size()-1));
        
      }
    }
    
    public void publish(Object o)
    {
      if (!isCancelled())
      {
        super.publish(o);
      }
    }
    
    public void cancel()
    {
      cancel(false);
      frame.dispose();
      
    }
      
    public void progress()
    {
      if (!isCancelled())
      {
        setProgress((totalDone*100)/totalWork);
      }
    }
    
    public void propertyChange(PropertyChangeEvent evt)
    {
      String strPropertyName = evt.getPropertyName();
      if ("progress".equals(strPropertyName))
      {
         barWidget.setValue((Integer)evt.getNewValue());
         statusLabel.setText(totalDone + "/" + totalWork);
      }
      
    }
    
    public void actionPerformed(ActionEvent arg0)
    {
      cancel();
    }


    
  }

}
