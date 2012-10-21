{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_synapse; 

interface

uses
    asn1util, blcksock, clamsend, dnssend, ftpsend, ftptsend, httpsend, 
  imapsend, ldapsend, mimeinln, mimemess, mimepart, nntpsend, pingsend, 
  pop3send, slogsend, smtpsend, snmpsend, sntpsend, synachar, synacode, 
  synacrypt, synadbg, synafpc, synaicnv, synaip, synamisc, synaser, synautil, 
  synsock, tlntsend, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('laz_synapse', @Register); 
end.
