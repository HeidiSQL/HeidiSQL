/// class used to store huge amount of data with fast retrieval
// - licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynBigTable;

(*
    Synopse Big Table. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse Big Table.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  What are these classes meant for?
  - add a simple storage and data persistence to your application, in a few KB of code
  - implement a logging or read/only audit trail mechanism into your application
  - access to compressed data on a CD-ROM or DVD-ROM (see our Open Source
    compression libraries in our web site) - you can even add items to the list,
    since they will remain in memory; but they will be lost when you close the file
  - have an easy way to share data between Delphi 7 and Delphi XE applications,
    without any Unicode / TStringList encoding headache
  - store external resource files (think about an hidden folder) in which you
    can have read and write access from your applications
  - store efficiently an huge number of records with fields, using
    TSynBigTableRecord: creating 1,000,000 records is less than 900 ms, with
    integrated indexes, low storage size and fast search possibilities
    (this class is much faster than any SQL engine)
  - store BLOB with some metadata fields associated, via TSynBigTableMetaData
    (can be used e.g. to store thunmbails of pictures, or HTML/XML pages,
    associated with some indexed data)
  - if you don't want to install a database engine and configure it
  - if you need speed and performance
  - if you don't want to reinvent the wheel
  - etc... etc...

  What are these classes NOT for?
  - interact with DBExpress or VCL database components
  - replacing NTFS - go to Linux and pick the right file system you need
  - storing hierarchical data (like directories) - use B-Tree and such instead
  - store huge data items (maximum is 1 GB for one item) - a few MB is good,
    but remember that data is stored into RAM before writting to disk
  - replace a SQL database engine - use our SQLite3 framework instead, see
    http://blog.synopse.info/category/Open-Source-Projects/SQLite3-Framework -
    but TSynBigTableMetaData and TSynBigTableRecord allow effective field
    handling, which could be sufficient as a simple database
  - save your soul or make me rich...


  Version 1.0
   - initial release

  Version 1.1
   - Fix save on disk issue, when some items are deleted but none added
   - enhanced unitary testing procedure

  Version 1.2
   - new TSynBigTableString class to store data from a UTF-8 encoded string ID
     instead of a numerical ID
   - added caching for last Get() items (may speed up next Get() a little bit)
   - custom Get() method for range retrieval into a dynamic array
   - TSynBigTable modified in order to handle custom data in header (used to
     store string IDs for TSynBigTableString for instance)
   - whole engine more robust against any file corruption or type mistmatch
   - Count property returned an incorrect value (including deleted values)
   - added timing (in 1/10 ms) for test steps
   - version 1.2b: even (much) faster TSynBigTableString.Add()

  Version 1.3
    - new Open() Read() and Seek() methods to read data like in a TStream
    - new Clear method to flush the table and rebuild from scratch
    - don't cache data bigger than 1 MB (to save RAM)

  Version 1.4
    - added RawByteStringFromFile() and FileFromRawByteString() procedures
    - added TSynBigTable.AddFile() method

  Version 1.7
    - Thread safe version of the Synopse Big Table

  Version 1.8
    - new GetPart() method for retrieving a part of a stored file
      (to be used especially for big file content)
    - fix issue with files > 2 GB (thanks to sanyin for the report)

  Version 1.9 - mostly requests and modification proposal from sanyin - thanks!
    - new TSynBigTable.GetLength() method
    - new TSynBigTable.ReadToStream() method
    - can set additional file open mode flags in TSynBigTable.Create
    - fixed an obscure possible issue for saving/loading TSynBigTableString
      with string IDs bigger in size than 65535 chars

  Version 1.9.2
    - Range Checking forced OFF to avoid problems with some projects
    - fFile type modified to THandle, instead of integer

  Version 1.12
    - this is a MAJOR update: the file format changed (new magics $ABAB0004/5)
    - now uses SynCommons unit (avoid too much duplicated code)
    - buffered writing and reading to file: major speed up of the unit,
      since Windows file access API are dead slow; for instance, reading
      uses now memory-mapped files for best possible performance
    - all previous caching (not working in fact) has been disabled
      (the caching is now implemented more efficiently at OS level, within
      memory mapped files)
    - TSynBigTableString has no 65535 key length limitation any more
    - values or UTF-8 keys of fixed-size are now stored in the most efficient way
    - new Update() methods, allowing to change the content of any record
    - new GetPointer() methods, to retrieve a pointer to the data, directly
      in memory mapped buffer (faster than a standar Get() call)
    - new GetAsStream() methods, to retrieve a data into an in-memory stream,
      pointing into the memory mapped buffer in most cases
    - new GetIterating() method, which will loop into all data items, calling
      a call-back with pointers to each data element (very fast method)
    - fDeleted[] array now stored in ascending order, to make whole unit faster
    - NumericalID[] property is now available also in TSynBigTable (don't use it
      to loop through all items, but rather the dedicated GetIterating() method)

  Version 1.12a
    - Offsets can now be stored as Int32 instead of Int64 (to save space in
      memory for most TSynBigTable usage): the file format did therefore changed
      (new magics $ABAB0006/7)
    - fixed issue when opening an TSynBigTableString file with updated items
    - fixed a major problem in TFileBufferWriter.WriteVarUInt32Array, with
      DataLayout=wkOffset and ValuesCount=1 - the same error occured in
      WriteVarUInt64DynArray with Offset=true and ValuesCount=1
    - fix issue with files >= 2 GB (thanks to sanyin for another report):
      memory mapped files are not well handled in this case -> direct read
      from disk is used for such huge files (but faster memory mapping is
      using for all files < 2 GB)
    - enhanced coherency checks when loading from a TSynBigTable file
    - the unit will now flushes the in-memory data to disk when 256 MB of data
      is stored in RAM (manual flush was confusing for some users) - see the
      new BIGTABLE_AUTOFLUSH_SIZE constant
    - code refactoring in TSynBigTableString: some generic functions were
      moved into SynCommons
    - get rid of the Open/Read/Seek methods: use GetAsStream() instead
    - faster GetIterating() method, looping in physical order in disk by default,
      and optionaly in Numerical ID order or even the faster available order
      (new TSynBigTableIterationOrder parameter)
    - added an Opaque parameter to GetIterating() method and corresponding
      TSynBigTableIterateEvent call-back
    - new GetAllIDs() method to retrieve all IDs at once
    - new GetAllPhysicalIndexes() method to retrive all physical indexes at once
    - new GetPointerFromPhysicalIndex() public method
    - faster Update: in-place refresh if previous data is still in memory
    - speed enhancements (e.g. TSynBigTable.Destroy) and extended test coverage
    - new TSynBigTableRecord class to store data, with fields handling inside
      each record, fast indexes and search possibilities, late-binding access
    - new TSynBigTableMetaData class to store BLOB data, with metadata fields in
      parallel (also with fast indexes and search, late-binding access)

  Version 1.12b
    - fixed issue with Pack method implementation, in case of updated records
    - fixed issue with TSynBigTableMetaData/TSynBigTableRecord updating and
      packing (and added corresponding regression tests)
    - AddField methods now return a boolean and no TSynTableFieldProperties,
      because these instances may be modified after a later AddFieldUpdate call
    - enhanced test coverage (mostly TSynBigTableMetaData/TSynBigTableRecord)

  Version 1.13
    - record validation now uses the generic TSynValidate mechanism, common
      with our main ORM framework, compliant with true multi-tier architecture:
      use Table.Validate method (see e.g. the update CheckConstraints)
    - record filtering now uses the generic TSynFilter mechanism, common
      with our main ORM framework, compliant with true multi-tier architecture:
      use Table.Filter method
    - GetAsStream() method now will create a TSynMemoryStreamMapped if needed,
      so will be faster than direct in-memory reading of the whole content
    - fixed issue when compiled with THREADSAFE

  Version 1.15
    - unit now tested with Delphi XE2 (32 Bit)

  Version 1.16
    - new overriden TSynBigTableTable.Clear method which will clear fields
    - new TSynBigTable.FileFormatCheck class method to recognize file format
    - replaced sbtGetMagic kind of header with InternalMagic class function
    - fixed issue in TSynTableFieldProperties.SaveTo about saving wrong indexes
    - new sbtBeforeWrite step available (e.g. to safely update indexes)

*)

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

{$ifndef LVCL}
{.$define THREADSAFE}
{ define this if you want to make the Synopse Big Table thread safe
  - we use internaly TMultiReadExclusiveWriteSynchronizer for better
   performance
  - not fully tested yet, just to ensure basic thread safety  }
{$endif}

{$R-}
{ Range Checking must be sent to OFF in the code below - code is safe anyway }
{$S-}
{ Stack Checking must be sent to OFF in the code below }

uses
  Windows,
  Classes,
  SysUtils,
  SynCommons;

type
  TSynBigTable = class;

  /// possible actions for CustomHeader() protected virtual method
  TSynBigTableCustomHeader = (sbtRead, sbtBeforeWrite, sbtWrite, sbtAfterRead);

  /// the way the GetIterating() method will loop through all items
  // - ioInternalID and ioInternalPhysical are internal codes used by
  // GetID and GetAllPhysicalIndexes methods
  TSynBigTableIterationOrder = (ioNone, ioPhysical, ioID, ioFaster,
    ioInternalID, ioInternalPhysical);

  /// prototype of a callback function for iterating through all items
  // of a table
  // - will be called following the incremental ID order
  // - implementation can set the result to TRUE to break the iteration loop
  // - the Data pointer is either direct access to the direct mapped buffer,
  // or a global temporary buffer: use the data between two iterations,
  // but copy the content if you need some persistency
  // - the DataIndex parameter is the index of the data, physicaly on disk
  TSynBigTableIterateEvent = function(Sender: TObject; Opaque: pointer;
    ID, DataIndex: integer; Data: pointer; DataLen: integer): boolean of object;

  /// event called after a pack, just before the UpdateToFile()
  // - can be used to synchronized the field indexes, e.g.
  // - format of supplied parameter is NewIndexs[oldIndex] := newIndex
  TSynBigTableAfterPackEvent = procedure(var NewIndexs: TIntegerDynArray) of object;

  /// a class to store huge amount of data, just specified by an integer ID
  // - data is stored in an unique file
  // - retrieval is very fast (can't be faster IMHO)
  // - data is appended at the end of this file at adding (but use a caching
  // mechanism for immediate adding)
  // - use a temporary in memory adding, till the UpdateToFile method is called
  // - data items can be deleted
  // - file can be packed using the Pack method in order to retrieve free space
  // from deleted entries (sounds like a VACUUM command, but faster)
  // - total size of file has no limit (but your hard disk, of course)
  // - limit of one data block depends on RAM (RawByteString is used
  // as storage for data block)
  // - before Delphi 2007, much faster when using FastMM4 memory manager
  // - after profiling, most of the time is spent in the Windows kernel,
  // waiting from hard disk write of raw data; in all cases, this class is
  // much faster than any SQL engine storing BLOB, and than plain Win32 files
  // - ACID behavior can be enforced by calling UpdateToFile(true)
  TSynBigTable = class
  private
    function IterateGetAllID(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    function IteratePack(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    /// used in TestBigTable function
    function TestIterateEvent(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    function TestIterateEventSpeed(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    /// retrieve a genuine integer value to identify the file content
    class function InternalMagic: integer; virtual;
  protected
    fFileName: TFileName;
    fCount: integer;
    fCurrentID: integer;
    fCurrentInMemoryDataSize: Int64;
    fInMemoryCount: integer;
    fInMemoryData: TRawByteStringDynArray;
    fInMemoryID: TIntegerDynArray;
    fDeletedCount: integer;
    fAliasCount: integer;
    fModified: boolean;
    fFile: THandle;
    /// all IDs, in the same order as the storage
    // - may contain an ID from a deleted entry -> don't use indexed with 0..Count-1
    // - Offset[] are in another dynamic array, so the code is somewhat faster
    fID: TIntegerDynArray;
    /// all end offsets of the storage data in the file (up to 1 GB of data)
    // - may contain an offset from a deleted entry -> don't use indexed with 0..Count-1
    // - ID[] are in another dynamic array, so the code is somewhat faster
    fOffset32: TIntegerDynArray;
    /// all end offsets of the storage data in the file (used for huge amount of data)
    // - may contain an offset from a deleted entry -> don't use indexed with 0..Count-1
    // - ID[] are in another dynamic array, so the code is somewhat faster
    fOffset64: TInt64DynArray;
    /// array of deleted items IDs
    // - this array is stored in ascending order, to make search faster
    fDeleted: TIntegerDynArray;
    /// array of Aliases for updated records
    fAliasSource, fAliasReal, fAliasRealSorted: TIntegerDynArray;
{$ifdef THREADSAFE}
    fLock: TMultiReadExclusiveWriteSynchronizer;
{$endif}
    fFileOpenMode: Cardinal;
    fReadBuffer: TFileBufferReader;
    fOnAfterPack: TSynBigTableAfterPackEvent;
    function GetCount: integer; virtual;
    /// this default implementation can be slow, because it must loop through all
    // items in case of deleted or updated items
    function GetID(Index: integer): integer; virtual;
    /// retrieve an index from a supplied ID
    // - return -1 if this ID is not existing in the index
    // - warning: return an index, even if the ID is in the fDeleted[] list
    // - but handle aliases IDs (after Update() method call)
    function IDToIndex(aID: integer; handleAliases: boolean=true): integer;
    /// retrieve an ID from a supplied index
    // - can handle aliases IDs (after Update() method call), i.e. return the
    // original ID, from the index of the updated content
    function IndexToID(aIndex: Integer; handleAliases: boolean=true): integer; virtual;
    /// retrieve a content, from its raw index in the database
    function GetFromIndex(aIndex: integer; out aData: RawByteString): boolean;
    /// retrieve a pointer to a content, from its raw index in the database
    function GetPointerFromIndex(aIndex: Integer; var aTempData: RawByteString;
      DataLen: PInteger=nil): pointer;
    /// load index from file into fID[], fMemory[] and fDeleted[]
    procedure LoadFromFile;
    /// returns true if the supplied file matches the current TSynBigTable class
    // - will allow making a difference between all implementation classes,
    // i.e. you can call TSynBigTable.FileFormatCheck,
    // TSynBigTableString.FileFormatCheck, TSynBigTableRecord.FileFormatCheck
    // or TSynBigTableMetaData.FileFormatCheck to verify a file layout
    // - uses the protected virtual InternalMagic method 
    class function FileFormatCheck(const aFileName: TFileName): boolean;
    /// virtual method which can be used to store additional data into the header
    // - do nothing by default in TSynBigTable (means no additional header data)
    // - if Action is sbtBeforeWrite, prepare the data before writing: at the
    // sbtWrite step, update data won't be available any more (fReadBuffer is
    // already closed so e.g. indexes should be recreated in this first step)
    // - if Action is sbtWrite, write the custom header data to WriteBuffer
    // - if Action is sbtRead, read the custom header data from fReadBuffer
    // - if Action is sbtAfterRead, all headers have been read from disk: the
    // class can therefore synchronize its content with the data (to recreate
    // any missing index, for instance)
    function CustomHeader(Action: TSynBigTableCustomHeader;
      WriteBuffer: TFileBufferWriter): integer; virtual;
    /// retrieve an offset for a specified physical ID
    // - read from either fOffset32[] either fOffset64[]
    function GetOffset(Index: integer): Int64;
    /// retrieve an index from a supplied ID, and the associated AliasReal[] index
    function InternalIDToIndex(aID: integer; var aAliasIndex: integer): integer;
    /// update the content, returing the updated physical ID 
    function InternalUpdateFromIndex(const aData: RawByteString;
      aID, aIndex, aAliasIndex: integer; var aNewIndex: cardinal): integer;
    /// will recreate the file processing all deleted or updated data, following
    // the ID order
    // - the callback must write the content (after any possible content change)
    // to the Opaque.WR file writer
    procedure RecreateFileContent(aCallBack: TSynBigTableIterateEvent;
      const OpaqueFields: TSQLFieldBits=[]);
  public
    /// initialize the database engine with a supplied filename
    // - InternalCacheSize can be used to customize the internal cache count
    // (set to 0 to disable caching; 128 is a good value, bigger makes no difference)
    // - you can specify a custom file open mode attributes on request, like
    // fmShareDenyNone (not set by default, for safety reason)
    constructor Create(const aFileName: TFileName; FileOpenMode: Cardinal = 0); reintroduce;
    /// finalize memory, and save all content
    destructor Destroy; override;
    /// clear the whole table content and indexs
    procedure Clear; virtual;
    /// add a data to the database
    // - return the unique ID created to identify this data
    // - you can force a specified ID number, by setting a non null value to
    // ForcedID (in this case, it MUST be added in increasing order)
    // - return 0 on error, otherwise the created Physical Index can be stored
    // - OldPhysicalIndex is to be used in case of constraint check (for
    // TSynBigTableRecord and TSynBigTableMetaData)
    function Add(const aData: RawByteString; ForcedID: integer=0;
      PhysicalIndex: PInteger=nil; OldPhysicalIndex: integer=-1): integer; virtual;
    /// add a file content to the database
    // - return the unique ID created to identify this data
    // - return 0 on error (e.g. specified file doesn't exist)
    function AddFile(const aFileName: TFileName): integer;
    /// retrieve a content, from a supplied ID
    // - return TRUE if found, FALSE if ID was not existing (or deleted)
    // - return the data into aData, or '' if ID was not existing (or deleted)
    function Get(aID: integer; out aData: RawByteString): boolean; overload;
    /// retrieve a content, from a supplied ID, into a TStream
    // - this method is faster than Get() into a RawByteString, because the
    // data is not moved from memory but mapped into a TCustomMemoryStream
    // - if the ID is not correct, returns nil
    // - if the ID is correct, returns a TStream instance, able to access to
    // the associated content
    // - in most cases, this TStream is just a wrapper around the memory mapped
    // buffer in memory
    // - the TStream must be consummed immediately, before any Pack or
    // UpdateToFile method calls
    // - the caller must Free the returned TStream instance
    // - if the data is not already memory mapped (i.e. for files >= 2 GB)
    // a custom TSynMemoryStreamMapped is used to access the data from disk
    function GetAsStream(aID: integer): TStream;
    /// retrieve the length of a content, from a supplied ID
    // - return -1 if the ID was not found, or the length (in bytes) of this ID content
    function GetLength(aID: integer): integer;
    /// retrieve a content, from a supplied ID, into a pointer
    // - returns nil on error
    // - returns a pointer to the data on success, directly from the memory mapped
    // file on most cases; if the data is not in a memory mapped buffer (i.e.
    // for files >= 2 GB) the aTempData variable is used to read the data from disk
    // - in case of success, if DataLen is not nil, it will be filled with the
    // corresponding data length
    // - this method is therefore much faster than Get()
    function GetPointer(aID: Integer; var aTempData: RawByteString;
      DataLen: PInteger=nil): pointer;
    /// retrieve a content, from a supplied ID, into a pointer
    // - returns nil on error
    // - returns a pointer to the data on success, directly from the memory mapped
    // file on most cases; if the data is not in a memory mapped buffer (i.e.
    // for files >= 2 GB) the aTempData variable is used to read the data from disk
    // - this method is not thread-safe (but is therefore faster)
    function GetPointerFromPhysicalIndex(aPhysicalIndex: integer;
      var aTempData: RawByteString): pointer;
    /// call a supplied Event method by iterating through all table items
    // - Event will be called following the physical order of data in the disk
    // file (somewhat faster), or incremental ID depending of Order parameter
    // - Event can set the result to TRUE to break the iteration loop
    // - the Opaque parameter will be supplied to the callback Event
    // - set DontRetrieveData to TRUE if you don't need any data to be set
    // in the callback, but only the ID (faster)
    procedure GetIterating(aCallBack: TSynBigTableIterateEvent;
      Order: TSynBigTableIterationOrder=ioPhysical; Opaque: pointer=nil;
      DontRetrieveData: Boolean=false); 
    /// fast retrieval of all IDs
    // - returned in physical or increasing ID value order
    // - returns the number of IDs stored in the integer array
    function GetAllIDs(var IDs: TIntegerDynArray;
      Order: TSynBigTableIterationOrder=ioPhysical): integer; virtual;
    /// fast retrieval of all used items physical indexes
    // - returned in physical order
    // - returns the number of Indexes stored in the integer array
    function GetAllPhysicalIndexes(var Indexes: TIntegerDynArray): integer;
    /// update a record content in the database
    // - in fact, a new invisible record is created, and an alias will map this
    // new record to the original ID
    // - the physical replacement will take place only during Pack method call
    // - returns the new content ID value on success
    // - returns 0 on error
    function Update(aID: Integer; const aData: RawByteString;
      PhysicalIndexOldNew: PInt64=nil): integer; virtual;
    /// retrieve a part of a file content
    // - faster than Open/Seek/Read methods, which loads the whole content in memory
    // before Seek and Read
    // - only the needed part of data is copied into aData
    function GetPart(aID: integer; Offset, Len: Integer; out aData: RawByteString): boolean;
    /// retrieve a list of content, from a supplied ID range (including boundaries)
    // - return TRUE if all were found, FALSE if some ID were not existing (or deleted)
    // - return the data into aData[], or '' if one particular ID was not
    // existing (or deleted); after call, length(aData)=aIDLast-aIDFirst+1
    function Get(aIDFirst, aIDLast: integer; out aData: TRawByteStringDynArray): boolean; overload;
    /// delete an ID
    function Delete(aID: integer; PhysicalIndex: PInteger=nil): boolean; virtual;
    /// pack the database, i.e. delete all formerly deleted ID from the disk
    // - if forceFlushOnDisk is TRUE, data is forced to be saved on the disk
    // (slower but allow ACID behavior of the database file)
    procedure Pack(forceFlushOnDisk: boolean=false);
    /// save last added entries into the files
    // - do nothing is nothing is to be written (if forceAlwaysWrite is false)
    // - can be called from time to time, after checking CurrentInMemoryDataSize
    // - if forceFlushOnDisk is TRUE, data is forced to be saved on the disk
    // (slower but allow ACID behavior of the database file)
    procedure UpdateToFile(forceFlushOnDisk: boolean=false;
      dontReopenReadBuffer: boolean=false);
    /// the entries count
    property Count: integer read GetCount;
    /// the associated filename storing the database
    property FileName: TFileName read fFileName;
    /// contain the current in memory data size (in bytes)
    // - i.e. the data size not written yet to the disk
    // - can be used to flush regularely the data to disk by calling UpdateToFile
    // method when this value reach a certain limit
    property CurrentInMemoryDataSize: Int64 read fCurrentInMemoryDataSize;
    /// retrieve an offset for a specified physical ID
    // - read from either fOffset32[] either fOffset64[]
    property Offset[Index: integer]: Int64 read GetOffset;
    /// read-only access to a numerical ID, from its index
    // - index is from NumericalID[0] to NumericalID[Count-1]
    // - follows the numerical ID order for TSynBigTable, and the alphabetical
    // order of UTF-8 keys for TSynBigTableString
    // - return 0 in case of out of range index
    // - this method can be slow with TSynBigTable (if they are some deleted or
    // updated items - just call the Pack method to improve speed); but with
    // a TSynBigTableString instance, it will be always fast
    // - don't use it to loop through all items, but rather the dedicated
    // GetIterating() or GetAllIDs() fast methods
    property NumericalID[Index: integer]: integer read GetID;
    /// event called after a pack, just before the UpdateToFile() call
    // - can be used to synchronized the field indexes, e.g.
    property OnAfterPack: TSynBigTableAfterPackEvent read fOnAfterPack write fOnAfterPack;
  end;

  /// a class to store huge amount of data, just specified by a string ID
  // - string ID are case-sensitive (important warning)
  // - string ID are of RawUTF8 type, so you must make explicit conversion
  // in your program to the native generic string type - you can use our
  // Utf8ToString() and StringToUtf8() functions, which work for
  // all version of Delphi (from Delphi 6 up to XE)
  TSynBigTableString = class(TSynBigTable)
  private
    function GetStringID(Index: integer): RawUTF8;
  protected
    /// = real Count property = fCount-fDeletedCount
    fHeaderCount: integer;
    /// store String IDs (in alpha-sorted order)
    fHeaderString: TRawUTF8DynArray;
    /// store associated numerical IDs (in alpha-sorted order)
    fHeaderID: TIntegerDynArray;
    /// overriden method which handle String ID loading and writing
    function CustomHeader(Action: TSynBigTableCustomHeader;
      WriteBuffer: TFileBufferWriter): integer; override;
    class function InternalMagic: integer; override;
    /// faster method using fHeaderID[]
    function GetID(Index: integer): integer; override;
    /// faster method returning fHeaderCount
    function GetCount: integer; override;
  public
    /// clear the whole table content and indexs
    procedure Clear; override;
    /// add a data to the database, and its associated string ID
    // - return the unique numerical ID created to identify this data
    // - return 0 if the string ID is invalid (i.e. void or already used)
    function Add(const aData: RawByteString; const aID: RawUTF8; ForcedID: integer=0): integer; reintroduce;
    /// retrieve a content, from a supplied string ID
    // - return TRUE if found, FALSE if this ID was not existing (or deleted)
    // - return the data into aData, or '' if ID was not existing (or deleted)
    function Get(const aID: RawUTF8; out aData: RawByteString): boolean; overload;
    /// retrieve a content, from a supplied ID, into a pointer
    // - returns nil on error
    // - returns a pointer to the data on success, directly from the memory mapped
    // file on most cases; if the data is not in a memory mapped buffer (i.e.
    // for files >= 2 GB) the aTempData variable is used to read the data from disk
    // - in case of success, if DataLen is not nil, it will be filled with the
    // corresponding data length
    // - this method is therefore much faster than Get() for big size of data
    function GetPointer(const aID: RawUTF8; var aTempData: RawByteString;
      DataLen: PInteger=nil): pointer; overload;
    /// retrieve a content, from a supplied ID, into a TStream
    // - if the ID is not correct, returns nil
    // - if the ID is correct, returns a TStream instance, able to access to
    // the associated content
    // - in most cases, this TStream is just a wrapper around the memory mapped
    // buffer in memory
    // - the TStream must be consummed immediately, before any Pack or
    // UpdateToFile method calls
    // - the caller must Free the returned TStream instance
    // - if the data is not already memory mapped (i.e. for files >= 2 GB)
    // a custom TSynMemoryStreamMapped is used to access the data from disk
    function GetAsStream(const aID: RawUTF8): TStream; overload;
    /// fast retrieval of all IDs
    // - this overriden method handle ioFaster order, i.e; the fHeaderID[] content
    // - returns the number of IDs stored in the integer array
    function GetAllIDs(var IDs: TIntegerDynArray;
      Order: TSynBigTableIterationOrder=ioPhysical): integer; override;
    /// delete an entry from its numerical ID
    function Delete(aID: integer; PhysicalIndex: PInteger=nil): boolean; overload; override;
    /// delete an entry from its string ID
    // - return true if the record was successfully deleted
    function Delete(const aID: RawUTF8): boolean; reintroduce; overload;
    /// update a record content in the database
    // - return true if the record was successfully updated
    function Update(const aData: RawByteString; const aID: RawUTF8): boolean; reintroduce; overload;
    /// retrieve a numerical ID from a UTF-8 encoded string ID
    // - return 0 if this string ID was not found
    function StringToID(const aID: RawUTF8): integer;
    /// retrieve the UTF-8 encoded string ID of a given numerical ID
    // - return '' if this ID was not found
    function IDToString(aID: integer): RawUTF8;
    /// read-only access to a string ID, from its index
    // - index is from StringID[0] to StringID[Count-1]
    // - string IDs are alphabetically sorted
    // - return '' in case of out of range index
    property StringID[Index: integer]: RawUTF8 read GetStringID;
  end;

  TSynBigTableTableClass = class of TSynBigTableTable;
  
  /// an abstract class, associating a TSynTable to a Big Table
  // - use optimized TSynTable logic for handling field values, using
  // our SBF compact binary format (similar to BSON or Protocol Buffers)
  TSynBigTableTable = class(TSynBigTable)
  protected
    fTableName: RawUTF8;
    fTable: TSynTable;
    /// used by Search() method below
    function IterateSearch(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    /// false e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function CheckConstraints(const aRecordData: RawByteString; RecordIndex: integer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// internal version used by Search() method
    procedure SearchIterating(aCallBack: TSynBigTableIterateEvent;
      Order: TSynBigTableIterationOrder; Opaque: pointer); virtual; abstract;
  public
    /// initialize the database engine with a supplied filename
    // - you can specify an internal Table Name, similar to SQL table name
    // - you should better call either TSynBigTableMetaData or TSynBigTableRecord
    // reintroduced constructor, which will set GetRecordData parameter as expected
    constructor Create(const aFileName: TFileName; const aTableName: RawUTF8;
       GetRecordData: TSynTableGetRecordData; FileOpenMode: Cardinal = 0); reintroduce;
    /// finalize memory, and save all content
    destructor Destroy; override;
    /// add a field description to the table
    // - just a wrapper to the Table.AddField method
    // - warning: caller must call the AddFieldUpdate method when all
    // AddField() methods have been called, in order to eventually process
    // all already existing data to the resulting new field order
    // - physical order does not necessary follow the AddField() call order:
    // for better performance, it will try to store fixed-sized record first,
    // multiple of 4 bytes first (access is faster if dat is 4 byte aligned),
    // then variable-length after fixed-sized fields; in all case, a field
    // indexed will be put first
    // - if tfoUnique is set in aOptions and there is already some data, this
    // method will raise an exception: it's not possible to a have multiple
    // void data unique, so it will always fail the constraint
    function AddField(const aName: RawUTF8; aType: TSynTableFieldType;
      aOptions: TSynTableFieldOptions=[]): boolean;
    /// this method must be called after calls to AddField/Table.AddField
    // methods
    // - this will launch the recreation of the database file content, if some
    // field were effectively added (to map the new field layout): in this case
    // some default void value is set for all newly added fields
    // - for TSynBigTableRecord, this method may recreate the field order then
    // reload all field instances: you must retrieve all TSynTableFieldProperties
    // instances after this method call via proper
    // ! aField := Table.Table['FieldName'];
    procedure AddFieldUpdate; virtual; abstract;
    /// search for a matching value in a given field
    // - add the matching IDs in ResultID[] (in sorted order, with no duplicate),
    // and update the number matching of elements in ResultIDCount (for performance
    // reasons, the ResultID[] array remains filled with 0 until
    // ResultID[ResultIDCount-1] itemk)
    // - will use any existing index, or will iterate through all data (slower)
    // if the ForceIterate parameter is either ioPhysical or ioID
    // - the Limit parameter is similar to the SQL LIMIT clause: if greater than 0,
    // an upper bound on the number of rows returned is placed (e.g. set Limit=1
    // to only retrieve the first match)
    function Search(Field: TSynTableFieldProperties; const WhereValue: TSBFString;
      var ResultID: TIntegerDynArray; var ResultIDCount: integer; Limit: Integer=0;
      ForceIterate: TSynBigTableIterationOrder=ioNone): boolean; overload;
{$ifndef LVCL}
    /// search for a matching value in a given field
    // - add the matching IDs in ResultID[] (in sorted order, with no duplicate)
    // - will use any existing index, or will iterate through all data (slower)
    // if the ForceIterate parameter is either ioPhysical or ioID
    // - the Limit parameter is similar to the SQL LIMIT clause: if greater than 0,
    // an upper bound on the number of rows returned is placed (e.g. set Limit=1
    // to only retrieve the first match)
    function Search(Field: TSynTableFieldProperties; const WhereValue: variant;
      var ResultID: TIntegerDynArray; var ResultIDCount: integer; Limit: Integer=0;
      ForceIterate: TSynBigTableIterationOrder=ioNone): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
{$endif}
    /// clear the whole table content and indexs
    // - also delete the field layout
    procedure Clear; override;
    /// the associated field description
    property Table: TSynTable read fTable;
    /// the internal Table Name
    property TableName: RawUTF8 read fTableName;
  public
{$ifndef LVCL}
    /// retrieve a void TSynTableVariantType variant instance
    // - similar to a call to Table.Data call
    function VariantVoid: Variant;
    /// retrieve a TSynTableVariantType variant to access a record properties
    function VariantGet(aID: integer): Variant; virtual; abstract;
{$endif}
    /// retrieve a record as a TSynTableData to access its properties
    // - using TSynTableData is faster than a TSynTableVariantType variant
    function RecordGet(aID: integer): TSynTableData; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a record as a TSynTableData to access its properties
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - this overloaded function doesn't use a function return, therefore
    // will avoid a Record copy content (faster)
    procedure RecordGet(aID: integer; var result: TSynTableData); overload; virtual; abstract;
    /// update a Record from a given TSynTableData content
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - aRecord.ID is used to identify the record for calling raw Update()
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint failure)
    // - for TSynBigTableMetaData, only update the metadata content, not the
    // main record content
    function RecordUpdate(const aDataRecord: TSynTableData): boolean; virtual; abstract;
  end;

  /// a class to store huge data (like files content), with metadata fields
  // associated with every record
  // - this class will store the fields in memory, then uses TSynBigTable records
  // to store some huge data blocks (e.g. file content), whereas TSynBigTableRecord
  // will store the fields in the records: TSynBigTableRecord is prefered for
  // huge number of records, and TSynBigTableMetaData is designed for less number
  // of records, but will natively handle associated "blob-like" data. For instance,
  // TSynBigTableRecord would be the right class to implement a logging table,
  // whereas TSynBigTableMetaData would be ideal for storing pictures.
  // - use optimized TSynTable logic for handling metadata field values, using
  // our SBF compact binary format (similar to BSON or Protocol Buffers)
  // - you can access to any metadata fields by using a custom
  // TSynTableVariantType variant type, allowing late-binding in the code
  // (this method is slower than direct access to the data due to the Variant
  // overhead, but is perhaps more convenient)
  TSynBigTableMetaData = class(TSynBigTableTable)
  protected
    fMetaDataCount: Integer;
    fMetaDataRecords: TRawByteStringDynArray;
    fMetaDataID: TIntegerDynArray;
    /// retrieve pointer to the metadata, SBF-encoded  
    function GetMetaData(aPhysicalIndex: integer; var aTempData: RawByteString): pointer;
    /// faster method using function[]
    function GetID(Index: integer): integer; override;
    /// retrieve an ID from a supplied index
    // - can handle aliases IDs (after Update() method call), i.e. return the
    // original ID, from the index of the updated content
    function IndexToID(aIndex: Integer; handleAliases: boolean=true): integer; override;
    /// faster method returning fHeaderCount
    function GetCount: integer; override;
    /// overriden method which handle field description loading and writing
    function CustomHeader(Action: TSynBigTableCustomHeader;
      WriteBuffer: TFileBufferWriter): integer; override;
    class function InternalMagic: integer; override;
    /// internal version used by Search() method
    // - only handle internal order by now (i.e. ioID)
    procedure SearchIterating(aCallBack: TSynBigTableIterateEvent;
      Order: TSynBigTableIterationOrder; Opaque: pointer); override;
  public
    /// initialize the database engine with a supplied filename
    // - you can specify an internal Table Name, similar to SQL table name
    constructor Create(const aFileName: TFileName; const aTableName: RawUTF8;
       FileOpenMode: Cardinal = 0); reintroduce;
    /// this method must be called after calls to AddField/Table.AddField
    // methods
    // - this will launch the recreation of the database file content, if some
    // field were effectively added (to map the new field layout): in this case
    // some default void value is set for all newly added fields
    // - for TSynBigTableMeta, this method may recreate the field order, but
    // won't change the TSynTableFieldProperties instances
    procedure AddFieldUpdate; override;
    /// retrieve the metadata record of a given ID, encoded in our SBF format
    // - it could be more convenient to use VariantGet() or even the faster
    // RecordGet() methods
    function GetMetaDataFromID(aID: integer): pointer;
    /// overriden method to delete an entry from its numerical ID
    // - this method will handle the metadata fields synchronization
    function Delete(aID: integer; PhysicalIndex: PInteger=nil): boolean; override;
    /// add a data item with its associated metadata record to the table
    // - the metadata record uses our SBF enconding, and is mandatory
    // - returns the unique ID created to identify this data
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed, or
    // if the supplied aMetaData is void)
    function Add(const aData: RawByteString; const aMetaData: TSBFString): integer; reintroduce; overload;
    /// update a metadata record using our SBF enconding
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint failure)
    // - this method will only update the meta data - the main record data must
    // be updated with the inherited Update() method
    function Update(aID: integer; const aMetaData: TSBFString): boolean; reintroduce; overload;
  public
{$ifndef LVCL}
    /// add a data item with its associated metadata record to the table
    // - the metadata record is a TSynTableVariantType variant
    // - returns the unique ID created to identify this data
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed)
    function VariantAdd(const aData: RawByteString; const aMetaDataRecord: Variant): integer;
    /// retrieve a TSynTableVariantType variant to access a record metadata
    function VariantGet(aID: integer): Variant; override;
    /// update a metadata record as TSynTableVariantType variant
    // - aRecord.ID is used to identify the record for calling raw Update()
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint
    // failure, or wrong variant type)
    // - this method will only update the meta data - the main record data must
    // be updated with the inherited Update() method
    function VariantUpdate(const aMetaDataRecord: Variant): boolean;
{$endif}
    /// add a record to the table, with associated meta data
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - return the unique ID created to identify this data
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed)
    function RecordAdd(const aData: RawByteString;
      const aMetaDataRecord: TSynTableData): integer;
    /// retrieve a record as a TSynTableData to access its properties
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - this overloaded function doesn't use a function return, therefore
    // will avoid a Record copy content (faster)
    procedure RecordGet(aID: integer; var result: TSynTableData); overload; override;
    /// update a Record from a given TSynTableData content
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - aRecord.ID is used to identify the record for calling raw Update()
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint failure)
    // - this method will only update the meta data - the main record data must
    // be updated with the inherited Update() method
    function RecordUpdate(const aMetaDataRecord: TSynTableData): boolean; override;
  end;

  /// a class to store huge amount of data, with fields in every record
  // - this class will store the fields in the TSynBigTable records, whereas
  // TSynBigTableMetaData will store the fields in memory, and will uses records
  // to store some huge data blocks (e.g. file content): TSynBigTableRecord is
  // prefered for huge number of records, and TSynBigTableMetaData is designed
  // for less records, but with associated "blob-like" data. For instance,
  // TSynBigTableRecord would be the right class to implement a logging table,
  // where TSynBigTableMetaData would be ideal for storing pictures.
  // - use optimized TSynTable logic for handling field values, using
  // our SBF compact binary format (similar to BSON or Protocol Buffers)
  // - you can access to any record content fields by using a custom
  // TSynTableVariantType variant type, allowing late-binding in the code
  // (this method is slower than direct access to the data due to the Variant
  // overhead, but is perhaps more convenient)
  TSynBigTableRecord = class(TSynBigTableTable)
  protected
    /// overriden method which handle field description loading and writing
    function CustomHeader(Action: TSynBigTableCustomHeader;
      WriteBuffer: TFileBufferWriter): integer; override;
    class function InternalMagic: integer; override;
    /// version used by Search() method, calling the default GetIterating method 
    procedure SearchIterating(aCallBack: TSynBigTableIterateEvent;
      Order: TSynBigTableIterationOrder; Opaque: pointer); override;
    /// refresh all fields indexes
    procedure RefreshIndexes(var NewIndexs: TIntegerDynArray);
  public
    /// initialize the database engine with a supplied filename
    // - you can specify an internal Table Name, similar to SQL table name
    constructor Create(const aFileName: TFileName; const aTableName: RawUTF8;
       FileOpenMode: Cardinal = 0); reintroduce;
    /// this method must be called after calls to AddField/Table.AddField
    // methods
    // - this will launch the recreation of the database file content, if some
    // field were effectively added (to map the new field layout): in this case
    // some default void value is set for all newly added fields
    // - for TSynBigTableRecord, this method may recreate the field order then
    // reload all field instances: you must retrieve all TSynTableFieldProperties
    // instances after this method call via proper
    // ! aField := Table.Table['FieldName'];
    procedure AddFieldUpdate; override;
  public
    /// overriden method to add a record to the database
    // - this method will handle the field indexes synchronization
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed)
    function Add(const aData: RawByteString; ForcedID: integer=0;
      PhysicalIndex: PInteger=nil; OldPhysicalIndex: integer=-1): integer; override;
    /// overriden method to delete an entry from its numerical ID
    // - this method will handle the field indexes synchronization
    function Delete(aID: integer; PhysicalIndex: PInteger=nil): boolean; override;
    /// overriden method to update a record content in the database
    // - returns 0 on updating error (e.g. if a tftUnique constraint failed)
    function Update(aID: Integer; const aData: RawByteString;
      PhysicalIndexOldNew: PInt64=nil): integer; override;
  public
{$ifndef LVCL}
    /// add a record to the table
    // - the record is a TSynTableVariantType variant
    // - returns the unique ID created to identify this data
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed)
    function VariantAdd(const aRecord: Variant): integer;
    /// retrieve a TSynTableVariantType variant to access a record properties
    function VariantGet(aID: integer): Variant; override;
    /// update a TSynTableVariantType variant
    // - aRecord.ID is used to identify the record for calling raw Update()
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint failure)
    function VariantUpdate(const aRecord: Variant): boolean;
{$endif}
    /// add a record to the table
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - return the unique ID created to identify this data
    // - you can specify an expected ID to be used in aForceID parameter
    // - returns 0 on adding error (e.g. if a tftUnique constraint failed)
    function RecordAdd(const aRecord: TSynTableData; aForcedID: integer=0): integer;
    /// retrieve a record as a TSynTableData to access its properties
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - this overloaded function doesn't use a function return, therefore
    // will avoid a Record copy content (faster)
    procedure RecordGet(aID: integer; var result: TSynTableData); overload; override;
    /// update a Record from a given TSynTableData content
    // - using TSynTableData is faster than a TSynTableVariantType variant
    // - aRecord.ID is used to identify the record for calling raw Update()
    // - returns TRUE on success, FALSE on error (e.g. tftUnique constraint failure)
    function RecordUpdate(const aRecord: TSynTableData): boolean; override;
  end;

  /// unitary testing of the SynBigTable unit
  TTestBigTable = class(TSynTestCase)
  protected
    FN: TFileName;
    Updated: TIntegerDynArray;
    UpdatedCount: integer;
    Deleted: TIntegerDynArray;
    class function CreateString(aID: integer; n: integer=0): RawByteString;
    function TestString(aID: integer; const Data: RawByteString): boolean;
    function TestStream(aID: integer; Data: TStream): boolean;
    function Delete(var T: TSynBigTable; n: integer; Pack,withString: boolean): boolean;
    function DoFieldTest(DoRecord: boolean; n: integer): boolean;
  published
    /// test TSynBigTable class
    procedure _TSynBigTable;
    /// test TSynBigTableString class
    procedure _TSynBigTableString;
    /// test TSynBigTableMetaData class
    procedure _TSynBigTableMetaData;
    /// test TSynBigTableRecord class
    procedure _TSynBigTableRecord;
  end;


/// unitary test function of the TSynBigTable class
// - return TRUE if test was OK, FALSE on any error
function TestBigTable: boolean;

const
  /// will flush the in-memory data to disk when reached 256 MB of data in RAM
  BIGTABLE_AUTOFLUSH_SIZE = 1 shl 28;


implementation


function TestBigTable: boolean;
procedure SetS(var s: RawUTF8; i: cardinal);
var p: array[0..3] of Word;
begin
  p[2] := TwoDigitLookupW[(i div 100)mod 100];
  p[3] := TwoDigitLookupW[i mod 100];
  i := i div 10000;
  p[0] := TwoDigitLookupW[i div 100];
  p[1] := TwoDigitLookupW[i mod 100];
  SetString(s,PAnsiChar(@p[0]),8);
end;
var
  Updated: TIntegerDynArray;
  UpdatedCount: integer;
function TestString(aID: integer; const Data: RawByteString): boolean;
var i: integer;
begin
  result := false;
  if FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,aID)>=0 then
    inc(aID,$01010101); // an updated ID has had its content refreshed
  for i := 0 to (length(Data) shr 2)-1 do
    if PIntegerArray(Data)^[i]<>aID then
      exit;
  result := true;
end;
function TestStream(aID: integer; Data: TStream): boolean;
var i: integer;
begin
  result := false;
  if not Data.InheritsFrom(TCustomMemoryStream) then
    exit;
  if FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,aID)>=0 then
    inc(aID,$01010101); // an updated ID has had its content refreshed
  with TCustomMemoryStream(Data) do
    for i := 0 to (Size shr 2)-1 do
      if PIntegerArray(Memory)^[i]<>aID then
        exit;
  result := true;
  Data.Free;
end;
var i,n,id: integer;
    T: TSynBigTable;
    TS: TSynBigTableString absolute T; // absolute T -> Delete() will work
    Data, Data2: RawByteString;
    FN: TFileName;
    Deleted: TIntegerDynArray;
    iStart,iStop,iFreq: Int64;
    filen: array[0..49] of RawUTF8;
    guid: TGUID;
procedure start(const Text: string);
begin
  if iStart<>0 then begin
    QueryPerformanceCounter(iStop);
    iStart := ((iStop-iStart)*(1000*10)) div iFreq;
    writeln('  ',iStart div 10,'.',iStart mod 10,' ms');
  end else
    writeln;
  if Text='' then begin
    iStart := 0;
    exit;
  end;
  write(Text);
  QueryPerformanceCounter(iStart);
end;
function Delete(Pack,withString: boolean): boolean;
var i,j,nu,id,tot: integer;
    St: TStream;
    tmp: TIntegerDynArray;
    ok: boolean;
begin
  result := false;
  SetLength(Deleted,Random(100)+10);
  fillchar(Deleted[0],length(Deleted)*4,0);
  start('Deleting '+IntToString(length(Deleted))+' elements');
  for i := 0 to high(Deleted) do
  repeat
    repeat
      id := Random(n)+1;
    until (T.IDToIndex(id)>=0) and
      (IntegerScanIndex(pointer(Deleted),length(Deleted),id)<0); // no dup
    Deleted[i] := id;
    ok := T.Delete(id);
  until ok;
  for i := 0 to high(Deleted) do
    if FastFindIntegerSorted(pointer(T.fDeleted),T.fDeletedCount-1,Deleted[i])<0 then
      exit;
  start('Iteration test');
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
   if tot<>T.Count then
    exit;
  if Pack then begin
    start('Packing');
    T.Pack; // delete data from disk
  end else begin
    start('Close');
    T.Free; // verify after read than fDelete[] are saved
    start('Open');
    if withString then
      TS := TSynBigTableString.Create(FN) else
      T := TSynBigTable.Create(FN);
  end;
  start('Verify');
  for i := 0 to high(Deleted) do
    if T.Get(Deleted[i],Data) then
      exit;
  if Pack then begin
    for i := 1 to n do
      if IntegerScanIndex(pointer(Deleted),length(Deleted),i)<0 then
      if not T.Get(i,Data) or not TestString(i,Data) then
        exit else
      if withString then
      if not TS.Get(Int32ToUTF8(i),Data) or not TestString(i,Data) then
        exit;
  end else
    for i := 1 to n do
      if T.Get(i,Data) and not TestString(i,Data) then
        exit else
      if withString then
      if TS.Get(Int32ToUTF8(i),Data) and not TestString(i,Data) then
        exit;
  nu := Random(100)+10;
  start('Updating '+IntToString(nu)+' elements');
  for i := 0 to nu-1 do
  repeat
    repeat
      id := Random(n)+1;
    until (T.IDToIndex(id)>=0) and
      (FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,id)<0); // no dup
    AddSortedInteger(Updated,UpdatedCount,id);
    T.Get(id,Data);
    UniqueString(AnsiString(Data));
    for j := 0 to (length(Data) shr 2)-1 do
      inc(PIntegerArray(Data)^[j],$01010101);
    if not TestString(id,Data) then
      exit;
//    if withString then
//      ok := TS.Update(Data,TS.IDToString(id)) else
    ok := T.Update(id,Data)<>0;
    if ok then
      if not T.Get(id,Data2) or (Data<>Data2) then
        exit;
  until ok;
  start('Verify');
  for i := 1 to n do
    if T.Get(i,Data) and not TestString(i,Data) then
      exit;
  start('AsStream');
  for i := 1 to n do begin
    St := T.GetAsStream(i);
    if (St<>nil) and not TestStream(i,St) then
      exit;
  end;
  start('Iteration test ID order');
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioID,@tot);
  if tot<>T.Count then
    exit;
  start('Iteration test physical order');
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
  if tot<>T.Count then
    exit;
  start('Iteration speed');
  T.GetIterating(T.TestIterateEventSpeed);
  start('ID[] speed');
  for i := 0 to T.Count-1 do
    if T.NumericalID[i]=0 then
      exit;
  start('GetAllIDs physical order');
  if T.GetAllIDs(tmp,ioPhysical)<>T.Count then
    exit;
  start('GetAllIDs ID order');
  if T.GetAllIDs(tmp,ioID)<>T.Count then
    exit;
  start('GetAllPhysicalIndexes');
  if T.GetAllPhysicalIndexes(tmp)<>T.Count then
    exit;
  result := true;
end;
var s,s2: RawByteString;
    st: string;
    nstr: string;
    By8: array of RawUTF8;
    tot: integer;
    tmp: TIntegerDynArray;
    rec: TSynTableData;
{$ifndef LVCL}
    p: pointer;
{$endif}
function stats(T: TSynBigTable): string;
begin
  result := Format('Values: %s, file size: %s',
    [KB(T.Offset[T.fCount-1]),KB(T.fReadBuffer.FileSize)]);
end;
function DoFieldTest(DoRecord: boolean): boolean;
var T: TSynBigTableTable;
    TRec: TSynBigTableRecord absolute T;
    TMeta: TSynBigTableMetaData absolute T;
function TRTest(TR: TSynBigTableTable): boolean;
var i: integer;
    {$ifndef LVCL}
    vari: variant;
    rec: TSynTableData;
    {$endif}
    fText, fInt: TSynTableFieldProperties;
    ID: TIntegerDynArray;
    IDCount: integer;
begin
  result := false;
  fText := TR.Table['Text'];
  fInt := TR.Table['INT'];
  {$ifndef LVCL}
  Start('Read as variant');
  for i := 1 to n do begin
    vari := TR.VariantGet(i);
    if (vari.ID<>i) or
       (vari.text<>By8[n-i]) or
       (vari.Int<>i-1) then
      exit;
  end;
  Start('Read as TSynTableData');
  for i := 1 to n do begin
    TR.RecordGet(i,rec);
    if (RawUTF8(rec.GetFieldValue(fText))<>By8[n-i]) or
       (integer(rec.GetFieldValue(fInt))<>i-1) then
      exit;
  end;
  Start('Read direct');
  for i := 1 to n do begin
    if DoRecord then
      p := TRec.GetPointer(i,s) else
      p := TMeta.GetMetaDataFromID(i);
    if (p=nil) or
       (fInt.GetInteger(p)<>i-1) or
       (fText.GetRawUTF8(p)<>By8[n-i]) then
      exit;
  end;  
  {$endif}
  Start('Search 50 Text iterating');
  IDCount := 0; // not mandatory (if ID=nil, TR.Search will do it)
  for i := n-50 to n-1 do
    if not TR.Search(fText,fText.SBF(By8[n-i]),ID,IDCount,0,ioID) or
      (IDCount<>1) or (ID[0]<>i) then
        exit else
        IDCount := 0;
  Start('Search 200 Text using index');
  for i := n-200 to n-1 do
    if not TR.Search(fText,fText.SBF(By8[n-i]),ID,IDCount) or
      (IDCount<>1) or (ID[0]<>i) then
        exit else
        IDCount := 0;
  Start('Search 50 Int iterating');
  for i := n-50 to n-1 do
    if not TR.Search(fInt,fInt.SBF(i),ID,IDCount,0,ioID) or
      (IDCount<>1) or (ID[0]<>i+1) then
        exit else
        IDCount := 0;
  Start('Search 200  Int using index');
  for i := n-200 to n-1 do
    if not TR.Search(fInt,fInt.SBF(i),ID,IDCount) or
      (IDCount<>1) or (ID[0]<>i+1) then
        exit else
        IDCount := 0;
  Result := true;
end;
var fText, fInt, fBool: TSynTableFieldProperties;
    i, nint, value: integer;
    ClassName: string;
    IDs: TIntegerDynArray;
    IDCount: integer;
begin
  result := false;
  if DoRecord then
    ClassName := TSynBigTableRecord.ClassName else
    ClassName := TSynBigTableMetaData.ClassName;
  FN := ChangeFileExt(paramstr(0),'.'+copy(ClassName,13,100));
  DeleteFile(FN);
  start(format(#13#10'Create a %s database',[ClassName]));
  if DoRecord then
    T := TSynBigTableRecord.Create(FN,'test') else
    T := TSynBigTableMetaData.Create(FN,'test');
  try
    T.AddField('text',tftWinAnsi,[tfoIndex]);
    T.AddField('Int',tftInt32,[tfoIndex,tfoUnique]);
    T.AddFieldUpdate;
    fText := T.Table['text']; // need to get it after AddFieldUpdate
    fInt := T.Table['int'];
    rec.Init(T.Table);
    start(format('Add %s records',[nstr]));
    for i := 0 to n-1 do begin
      rec.SetFieldSBFValue(fText,fText.SBF(By8[n-i-1])); // same as rec.Field['text'] := 
      rec.SetFieldSBFValue(fInt,fInt.SBF(i));
      if DoRecord and (TRec.RecordAdd(rec)<>i+1) then
        exit else
      if not DoRecord and (TMeta.RecordAdd(By8[n-i-1],rec)<>i+1) then
        exit;
    end;
    if tfoUnique in fInt.Options then begin
      start(format('Try to add %d records with not unique field',[n shr 3]));
      for i := 0 to (n shr 3)-1 do begin
        rec.SetFieldSBFValue(fInt,fInt.SBF(i shl 3));
        if DoRecord and (TRec.RecordAdd(rec)<>0) then
          exit else
        if not DoRecord and (TMeta.RecordAdd(By8[n-i],rec)<>0) then
          exit;
      end;
    end;
    if not TRTest(T) then exit;
    start('UpdateToFile');
    T.UpdateToFile;
    start('Close');
    T.Free;
    start('Open');
    if DoRecord then
      T := TSynBigTableRecord.Create(FN,'test') else
      T := TSynBigTableMetaData.Create(FN,'test');
    if not TRTest(T) then exit; 
    start('Add a field');
    T.AddField('bool',tftBoolean);
    start('Recreate file with new field layout');
    T.AddFieldUpdate;
    if not TRTest(T) then exit;
    start(format('%d record content update',[n shr 3]));
    fBool := T.Table['bool']; // need to get it after any AddFieldUpdate
    for i := 0 to (n shr 3)-1 do begin
      //write(i:10,'        '#13);
      rec := T.RecordGet(i shl 3+1);
      if rec.ID<>i shl 3+1 then exit;
      rec.SetFieldSBFValue(fBool,fBool.SBF(true));
      if not T.RecordUpdate(rec) then exit;
      if not fBool.GetBoolean(Pointer(rec.SBF)) then exit;
    end;
    start('Packing');
    T.Pack;
    fText := T.Table['text'];
    if (fText=nil) or not TRTest(T) then exit;
    start(format('%d record text field content update',[n shr 5]));
    for i := 0 to (n shr 5)-1 do begin
      id := i shl 5+1;
      rec := T.RecordGet(id);
      if rec.ID<>id then exit;
      By8[n-id] := Int32ToUTF8(id); // as expected by TRTest above
      rec.SetFieldSBFValue(fText,fText.SBF(By8[n-id]));
      if not T.RecordUpdate(rec) then exit;
      if fText.GetRawUTF8(Pointer(rec.SBF))<>By8[n-id] then exit;
    end;
    fInt := T.Table['int'];
    if (fInt=nil) or not TRTest(T) then exit;
    nInt := 500; // unique field update is slow by design, need to refresh index 
    start(format('%d record unique Int field content update',[nint]));
    for i := 0 to nint-1 do begin
      id := i shl 3+1;
      rec := T.RecordGet(id);
      if rec.ID<>id then exit;
      rec.SetFieldSBFValue(fInt,fInt.SBF(n*2+id)); // > n -> so unique value
      if not T.RecordUpdate(rec) then exit;
      if fInt.GetInteger(pointer(rec.SBF))<>n*2+id then exit;
    end;
    start(format('Test unique Int index after update for %s records',[nstr]));
    for i := 0 to n-1 do begin
      if (i and 7=0) and (i<nint shl 3) then
        Value := n*2+i+1 else
        Value := i;
      IDCount := 0;
      if not T.Search(fInt,fInt.SBF(Value),IDs,IDCount) or
        (IDCount<>1) or (IDs[0]<>i+1) then
{      if not T.Search(fInt,fInt.SBF(Value),IDs,IDCount,0,ioPhysical) or
        (IDCount<>1) or (IDs[0]<>i+1) then }
          exit;
    end;
    start(stats(T));
    result := true;
  finally
    T.Free;
  end;
end;
begin
  result := false;
  iStart := 0;
  QueryPerformanceFrequency(iFreq);
  Randomize;
  //RandSeed := 51672226;
  //writeln('RandSeed=',RandSeed);
  {$ifndef LVCL}
    {$ifdef ISDELPHIXE2}FormatSettings.{$endif}
      ThousandSeparator := ','; // to avoid buggy characters on console
  {$endif}
  n := 100000;
  nstr := format('%.0n',[n*1.0]);
  start(format(#13#10'Create %s records data in memory',[nstr]));
  SetLength(By8,n);
  for i := 1 to n do
    SetS(By8[i-1],i);
  if true then
  if not DoFieldTest(false) then // TSynBigTableMetaData test
    exit else
  if not DoFieldTest(true) then  // TSynBigTableRecord test
    exit;
{  start(format('Create %s records data in memory',[nstr]));
  for i := 1 to n do SetS(By8[i-1],i);}
  FN := ChangeFileExt(paramstr(0),'.syn1');
  DeleteFile(FN);
  if true then begin
    TS := TSynBigTableString.Create(FN,0);
    try
      SetLength(s,8);
      Start(format(
        #13#10'Store %s records of 8 chars key / 8 chars values',[nstr]));
      for i := 0 to n-1 do begin
        if TS.Add(By8[i],By8[i])<>i+1 then
          exit;
      end;
      Start('Read');
      for i := 1 to n do
        if not TS.Get(i,s2) then
          exit;
      start('UpdateToFile');
      TS.UpdateToFile;
      start('Close');
      TS.Free;
      start('Open');
      TS := TSynBigTableString.Create(FN);
      start('Verify');
      for i := 1 to n do
        if not TS.Get(i,s2) then
          exit;
      start('Iteration test in physical order');
      tot := 0;
      TS.GetIterating(TS.TestIterateEvent,ioPhysical,@tot);
      if tot<>TS.Count then
        exit;
      start('Iteration test in ID order');
      tot := 0;
      TS.GetIterating(TS.TestIterateEvent,ioID,@tot);
      if tot<>TS.Count then
        exit;
      start('Iteration speed');
      TS.GetIterating(T.TestIterateEventSpeed);
      start('ID[] speed');
      for i := 0 to TS.Count-1 do
        if TS.NumericalID[i]=0 then
          exit;
      start('GetAllIDs physical order');
      if TS.GetAllIDs(tmp,ioPhysical)<>T.Count then
        exit;
      start('GetAllIDs ID order');
      if TS.GetAllIDs(tmp,ioID)<>T.Count then
        exit;
      start('GetAllIDs faster order');
      if TS.GetAllIDs(tmp,ioFaster)<>T.Count then
        exit;
      start('GetAllPhysicalIndexes');
      if TS.GetAllPhysicalIndexes(tmp)<>T.Count then
        exit;
      start(stats(TS));
    finally
      TS.Free;
    end;
  end;
//  RandSeed := 1000;
  start('');
  Finalize(By8);
  n := Random(50)*50+1500;
  UpdatedCount := 0;
  FN := ChangeFileExt(paramstr(0),'.syn');
  DeleteFile(FN);
  start('Creating a TSynBigTable with '+IntToString(n)+' elements');
  T := TSynBigTable.Create(FN);
  if true then
  try
    for i := 1 to n do
      if T.Add(TTestBigTable.CreateString(i))<>i then
        exit else
      if T.CurrentInMemoryDataSize>10 shl 20 then // write on disk every 10 MB
        T.UpdateToFile;
    if T.Count<>n then
      exit;
    start('Verify');
    for i := 1 to n do
      if not T.Get(i,Data) or not TestString(i,Data) then
        exit;
    start('Close and Open');
    T.Free;
    T := TSynBigTable.Create(FN); // verify after read
    start('Verify Random');
    for i := 1 to n do begin
      id := Random(n)+1; // test random access speed
      if not T.Get(id,Data) or not TestString(id,Data) then
        exit;
    end;
    start('Verify');
    for i := 1 to n do
      if not T.Get(i,Data) or not TestString(i,Data) then
        exit;
    start('Adding 1000 elements');
    for i := n+1 to n+1000 do // create some fInMemory[] data before Pack
      if T.Add(TTestBigTable.CreateString(i))<>i then
        exit;
    start('Updating 1000 elements');
    for i := n+1000 downto n+1 do // update fInMemory[] in-place data
      if T.Update(i,TTestBigTable.CreateString(i))<>i then
        exit;
    inc(n,1000);
    if not Delete(true,false) then
      exit;
    start('Close and Open');
    T.Free;
    T := TSynBigTable.Create(FN); // verify after read
    start('Verify');
    for i := 1 to n do
      if IntegerScanIndex(pointer(Deleted),length(Deleted),i)<0 then
      if not T.Get(i,Data) or not TestString(i,Data) then
      if not T.Get(i,Data) or not TestString(i,Data) then
        exit;
    if not Delete(false,false) then
      exit;
    start('Packing');
    T.Pack; // delete data from disk
    start('Iteration test physical order');
    tot := 0;
    T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
    if tot<>T.Count then
      exit;
    start('Iteration test ID order');
    tot := 0;
    T.GetIterating(T.TestIterateEvent,ioID,@tot);
    if tot<>T.Count then
      exit;
    start('Iteration speed');
    T.GetIterating(T.TestIterateEventSpeed);
    start('ID[] speed');
    for i := 0 to T.Count-1 do
      if T.NumericalID[i]=0 then
        exit;
    start(stats(T));
  finally
    T.Free;
  end;
  start('');
  dec(n,1000);
  FN := FN+'2';
  UpdatedCount := 0;
  DeleteFile(FN);
  TS := TSynBigTableString.Create(FN);
  try
    start('Creating a GUID-indexed TSynBigTableString with '+IntToString(Length(FileN))+' * 4MB elements');
    for i := 0 to high(FileN) do
      repeat // avoid duplicates (Add=0 if FileN[i] was already there)
        guid.D1 := Random(maxInt);
        guid.D2 := Random(65535);
        guid.D3 := Random(65535);
        Int64(guid.D4) := Int64(Random(maxInt))*Random(maxInt);
        FileN[i] := RawUTF8(GuidToString(guid));
      until TS.Add(TTestBigTable.CreateString(i+1,1 shl 20),FileN[i])<>0; // 4 MB each entry
    start('Verify');
    for i := high(FileN) downto 0 do
      if not TS.Get(FileN[i],data) or not TestString(i+1,Data) then
        exit;
    start('Close');
    TS.Free;
    start('Open');
    TS := TSynBigTableString.Create(FN);
    start('Verify');
    for i := 0 to high(FileN) do
      if not TS.Get(FileN[i],data) or not TestString(i+1,Data) then
        exit;
    start('Delete');
    id := Random(length(FileN));
    TS.Delete(FileN[id]);
    start('Close and Open');
    TS.Free;
    TS := TSynBigTableString.Create(FN);
    start('Pack');
    TS.Pack;
    start('Add one & UpdateToFile');
    TS.Add(TTestBigTable.CreateString(777,2 shl 20),FileN[id]);
    TS.UpdateToFile;
    start('Verify');
    for i := 0 to high(FileN) do
      if i<>id then
        if not TS.Get(FileN[i],data) or not TestString(i+1,Data) then
          exit;
    if not TS.Get(FileN[id],data) or not TestString(777,Data) then
      exit;
    st := stats(T);
    start('Clear');
    TS.Clear;
    UpdatedCount := 0;
    start(st+#13#10#10'Creating a string-indexed TSynBigTableString with '+
      IntToString(n)+' elements');
    for i := 1 to n do
      if TS.Add(TTestBigTable.CreateString(i),Int32ToUTF8(i))<>i then
        exit else
      if TS.CurrentInMemoryDataSize>10 shl 20 then // write on disk every 10 MB
        TS.UpdateToFile;
    if TS.Count<>n then
      exit;
    start('Verify');
    for i := 1 to n do
      if not TS.Get(i,Data) or not TestString(i,Data) or
         not TS.Get(Int32ToUTF8(i),Data) or not TestString(i,Data) then
        exit;
    start('Close and Open');
    TS.Free;
    TS := TSynBigTableString.Create(FN); // verify after read
    start('Verify');
    for i := 1 to n do
      if not TS.Get(i,Data) or not TestString(i,Data) or
         not TS.Get(Int32ToUTF8(i),Data) or not TestString(i,Data) then
        exit;
    start('Verify Random');
    for i := 1 to n do begin
      id := Random(n)+1; // test random access
      if not TS.Get(id,Data) or not TestString(id,Data) or
         not TS.Get(Int32ToUTF8(id),Data) or not TestString(id,Data) then
        exit;
    end;
    start('Adding 1000 elements');
    for i := n+1 to n+1000 do // create some fInMemory[] data before Pack
      if TS.Add(TTestBigTable.CreateString(i),Int32ToUTF8(i))<>i then
        exit;
    inc(n,1000);
    start('Verify');
    for i := 1 to n do
      if not TS.Get(i,Data) or not TestString(i,Data) or
         not TS.Get(Int32ToUTF8(i),Data) or not TestString(i,Data) then
        exit;
    if not Delete(true,true) then
      exit;
    start('Close and Open');
    TS.Free;
    TS := TSynBigTableString.Create(FN); // verify after read
    if not Delete(false,true) then
      exit;
    start(stats(T));
    writeln;
    result := true;
  finally
    TS.Free;
  end;
end;


{ TSynBigTable }

function TSynBigTable.Add(const aData: RawByteString; ForcedID: integer=0;
   PhysicalIndex: PInteger=nil; OldPhysicalIndex: integer=-1): integer;
var n: integer;
begin
  if (self=nil) or (fFile=0) then begin
    result := 0; // mark error (mostly file format mismatch)
    exit;
  end;
{$ifdef THREADSAFE}
  fLock.BeginWrite;
  try
{$endif}
    n := length(fInMemoryData);
    //assert(length(fInMemoryID)=n);
    if fInMemoryCount>=n then begin // reserve enough memory
      inc(n,128+n shr 3);
      SetLength(fInMemoryData,n);
      SetLength(fInMemoryID,n);
    end;
    if ForcedID<>0 then
      if ForcedID<fCurrentID then begin
        result := 0; // invalid decreased ID -> abort
        exit;
      end else
      fCurrentID := ForcedID else
      inc(fCurrentID); // create increasing IDs -> can use fast binary search
    result := fCurrentID;
    fModified := true; // mark need to be saved
    fInMemoryID[fInMemoryCount] := fCurrentID;
    fInMemoryData[fInMemoryCount] := aData;
    if PhysicalIndex<>nil then
      PhysicalIndex^ := fInMemoryCount+fCount;
    inc(fInMemoryCount);
    inc(fCurrentInMemoryDataSize,PtrUInt(length(aData)));
    if fCurrentInMemoryDataSize>BIGTABLE_AUTOFLUSH_SIZE then
      UpdateToFile; // force flush the in-memory data to disk if > 256 MB
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;

function TSynBigTable.AddFile(const aFileName: TFileName): integer;
var tmp: RawByteString;
begin
  tmp := StringFromFile(aFileName);
  if tmp<>'' then
    result := Add(tmp) else
    result := 0;
end;

procedure TSynBigTable.Clear;
begin
{$ifdef THREADSAFE}
   fLock.BeginWrite;
   try
{$endif}
    fReadBuffer.Close;
    if fFile<>0 then
      FileClose(fFile);
    DeleteFile(fFileName);
    LoadFromFile; // will reset all internal data
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;

constructor TSynBigTable.Create(const aFileName: TFileName; FileOpenMode: Cardinal=0);
var magic: Integer;
begin
{$ifdef THREADSAFE}
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
{$endif}
  fFileOpenMode := FileOpenMode;
  fFileName := aFileName;
  magic := InternalMagic;
  if ((magic=integer($ABAB0006)) and (PPointer(self)^<>TSynBigTable)) or
     ((magic=integer($ABAB0007)) and (PPointer(self)^<>TSynBigTableString)) or
     ((magic=integer($A5A50001)) and (PPointer(self)^<>TSynBigTableRecord)) or
     ((magic=integer($A5A50002)) and (PPointer(self)^<>TSynBigTableMetaData)) then
    raise Exception.CreateFmt('%s shall have its own InternalMagic',[ClassName]);
  LoadFromFile;
end;

function TSynBigTable.CustomHeader(Action: TSynBigTableCustomHeader;
  WriteBuffer: TFileBufferWriter): integer;
begin
  result := 0;
end;

function TSynBigTable.Delete(aID: integer; PhysicalIndex: PInteger): boolean;
var i,n,ndx: integer;
begin
  result := false;
  if (self=nil) or (aID<=0) or (fFile=0) then
    exit;
{$ifdef THREADSAFE}
  fLock.BeginWrite;
  try
{$endif}
    if IntegerScan(pointer(fAliasReal),fAliasCount-1,aID)<>nil then
      exit; // do NOT delete an updated alias, which is an hidden value
    ndx := IDToIndex(aID,true);
    if ndx<0 then // aID not found
      exit;
    if PhysicalIndex<>nil then
      PhysicalIndex^ := ndx;
    if AddSortedInteger(fDeleted,fDeletedCount,aID)<0 then
      exit; // already deleted
    result := true;
    fModified := true; // mark need to save file index
    i := FastFindIntegerSorted(pointer(fAliasSource),fAliasCount-1,aID);
    if i>=0 then begin
      AddSortedInteger(fDeleted,fDeletedCount,fAliasReal[i]);
      n := fAliasCount;
      DeleteInteger(fAliasSource,n,i);
      DeleteInteger(fAliasReal,fAliasCount,i);
      if fAliasRealSorted<>nil then
        SetLength(fAliasRealSorted,0);
    end;
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;

destructor TSynBigTable.Destroy;
begin
  UpdateToFile(false,true);
  fReadBuffer.Close;
  if fFile<>0 then
    FileClose(fFile);
{$ifdef THREADSAFE}
  fLock.Free;
{$endif}
  inherited;
end;

function TSynBigTable.Get(aID: integer; out aData: RawByteString): boolean;
begin
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    // aData := ''; already done by caller (because of "out aData" definition)
    if (self=nil) or (fFile=0) or (aID<=0) or
       ((fDeletedCount>0) and (aID>=fDeleted[0]) and
        (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0)) then
      result := false else
      result := GetFromIndex(IDToIndex(aID),aData);
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.Get(aIDFirst, aIDLast: integer;
  out aData: TRawByteStringDynArray): boolean;
var aID: integer;
begin
  result := true; // OK by default
  if aIDLast<aIDFirst then // avoid GPF
    Finalize(aData) else begin
    SetLength(aData,aIDLast-aIDFirst+1);
    for aID := aIDFirst to aIDLast do
      if not Get(aID,aData[aID-aIDFirst]) then
        result := false; // not found -> leave aData[]='' and return false
  end;
end;

type
  PIterateGetDynArrayIntegerOpaque = ^TIterateGetDynArrayIntegerOpaque;
  TIterateGetDynArrayIntegerOpaque = record
    Values: PIntegerDynArray;
    Count: Integer;
  end;

function TSynBigTable.GetAllIDs(var IDs: TIntegerDynArray;
  Order: TSynBigTableIterationOrder=ioPhysical): integer;
var tmp: TIterateGetDynArrayIntegerOpaque;
begin
  tmp.Values := @IDs;
  tmp.Count := 0;
  GetIterating(IterateGetAllID,Order,@tmp,true);
  result := tmp.Count;
end;

function TSynBigTable.GetAllPhysicalIndexes(var Indexes: TIntegerDynArray): integer;
var tmp: TIterateGetDynArrayIntegerOpaque;
begin
  tmp.Values := @Indexes;
  tmp.Count := 0;
  SetLength(Indexes,Count);
  GetIterating(nil,ioInternalPhysical,@tmp,true);
  result := tmp.Count;
  assert(result=length(Indexes));
end;

function TSynBigTable.GetAsStream(aID: integer): TStream;
var aIndex: integer;
    OffsetBegin: Int64;
begin
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    if (self=nil) or (fFile=0) or (aID<=0) or
       (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0) then
      result := nil else begin
      aIndex := IDToIndex(aID);
      if aIndex<0 then
        result := nil else
      if aIndex<fCount then begin
        // retrieve from file
        OffsetBegin := Offset[aIndex-1];
        fReadBuffer.Seek(OffsetBegin);
        result := fReadBuffer.ReadStream(Offset[aIndex]-OffsetBegin);
      end else
        // retrieve from data not already written to disk
        result := TSynMemoryStream.Create(fInMemoryData[aIndex-fCount]);
    end;
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.GetCount: integer;
begin
  if (self=nil) or (fFile=0) then // avoid GPF
    result := 0 else begin
{$ifdef THREADSAFE}
    fLock.BeginRead;
    try
{$endif}
      result := fCount+fInMemoryCount-fDeletedCount-fAliasCount;
{$ifdef THREADSAFE}
    finally
      fLock.EndRead;
    end;
{$endif}
  end;
end;

function TSynBigTable.GetFromIndex(aIndex: integer;
  out aData: RawByteString): boolean;
var Len: integer;
    OffsetBegin: Int64;
begin
  if aIndex<0 then
    result := false else begin
    if aIndex<fCount then begin
      // retrieve from file
      OffsetBegin := Offset[aIndex-1];
      fReadBuffer.Seek(OffsetBegin);
      Len := Offset[aIndex]-OffsetBegin;
      SetLength(aData,Len);
      fReadBuffer.Read(pointer(aData),Len);
    end else
      // retrieve from data not already written to disk
      aData := fInMemoryData[aIndex-fCount];
    result := true;
  end;
end;

function TSynBigTable.GetID(Index: integer): integer;
var tmp: integer;
begin
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
   if (self<>nil) and (cardinal(Index)<cardinal(fCount+fInMemoryCount-fDeletedCount-fAliasCount)) then begin
     if Index<fCount then
       result := fID[Index] else
       result := fInMemoryID[Index-fCount];
     if ((fDeletedCount=0) or (fDeleted[0]>result)) and
        ((fAliasCount=0) or (fAliasSource[0]>result)) then
       exit; // best case: no deleted nor updated item before this index
     tmp := Index;
     GetIterating(nil,ioInternalID,@tmp); // will loop through all IDs till reached tmp=0
     if tmp>index then begin
       result := tmp;
       exit;
     end;
   end;
   result := 0;
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

procedure TSynBigTable.GetIterating(aCallBack: TSynBigTableIterateEvent;
  Order: TSynBigTableIterationOrder=ioPhysical; Opaque: pointer=nil;
  DontRetrieveData: Boolean=false);
type TStopper = (none,deleted,updated,alias);
var i,j, n, aID, index: integer;
    stopID: Cardinal;
    DataLen: integer;
    aTempData: RawByteString;
    stopper, S: TStopper;
    next: array[deleted..alias] of integer;
    stop: array[deleted..alias] of cardinal;
    GetID: PInteger absolute Opaque;
    Physical: PIterateGetDynArrayIntegerOpaque absolute Opaque; 
label CallBack;
begin
  if self=nil then
    exit;
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    n := fCount+fInMemoryCount;
    FillChar(next,sizeof(next),0);
    stopID := 0;
    stopper := none;
    j := 0;
    if fAliasCount>0 then
      if fAliasRealSorted=nil then
        CopyAndSortInteger(Pointer(fAliasReal),fAliasCount,fAliasRealSorted);
    while j<n do begin
      // retrieve the next stopper item
      if stopID<=Cardinal(fCurrentID) then begin
        fillchar(stop,sizeof(stop),255); // set all stop[]>cardinal(maxInt);
        if next[deleted]<fDeletedCount then
          stop[deleted] := fDeleted[next[deleted]];
        if (Order<>ioInternalID) and (next[updated]<fAliasCount) then
          stop[updated] := fAliasSource[next[updated]];
        if (Order<>ioInternalPhysical) and (next[alias]<fAliasCount) then
          stop[alias] := fAliasRealSorted[next[alias]];
        stopID := maxInt; // no more stopper
        for S := deleted to alias do
          if stop[S]<stopID then begin
            stopID := stop[S];
            stopper := S;
          end;
      end;
      // process data until reached stopper
      case Order of
      ioInternalID:
      for i := j to n-1 do begin
        j := i+1;
        if i<fCount then
          aID := fID[i] else
          aID := fInMemoryID[i-fCount];
        if aID=integer(stopID) then begin
          inc(next[stopper]); // find next fDeleted[] or fAliasReal[]
          break; // we reached StopID -> get next udpated or deleted ID
        end else
        if GetID^>0 then
          dec(GetID^) else begin
          GetID^ := aID; // reached index -> return corresponding ID
          exit;
        end;
      end;
      ioInternalPhysical:
      if stopID=cardinal(maxInt) then begin
        // no more stopper -> fastest possible method
        dec(n,j);
        FillIncreasing(@Physical^.Values^[Physical^.Count],j,n);
        inc(Physical^.Count,n);
        exit;
      end else
      for i := j to n-1 do begin
        j := i+1;
        if i<fCount then
          aID := fID[i] else
          aID := fInMemoryID[i-fCount];
        if aID=integer(stopID) then begin
          inc(next[stopper]); // find next fDeleted[] or fAliasSource[]
          break; // we reached StopID -> get next udpated or deleted ID
        end else begin
          Physical^.Values^[Physical^.Count] := i; // SetLength(Values^,Count) was made
          inc(Physical^.Count);
        end;
      end;
      else
      if Assigned(aCallBack) then
      for i := j to n-1 do begin
        j := i+1;
        if i<fCount then
          aID := fID[i] else
          aID := fInMemoryID[i-fCount];
        if aID=integer(stopID) then begin
          case stopper of
          alias: // ID: ignore alias, physical: callback expects original ID
            if Order=ioPhysical then begin
              aID := fAliasSource[IntegerScanIndex(pointer(fAliasReal),fAliasCount,aID)];
              if DontRetrieveData then begin
                if not aCallBack(self,Opaque,aID,0,nil,0) then
                  exit; // forced iteration break
              end else begin
                index := i;
                goto CallBack;
              end;
            end;
          updated:
            // physical: ignore update, ID: callback expects updated data
            if Order=ioID then
            if DontRetrieveData then begin
              if not aCallBack(self,Opaque,aID,0,nil,0) then
                exit; // forced iteration break
            end else begin
              index := IDToIndex(fAliasReal[next[updated]],false);
              if index>=0 then
CallBack:       if not aCallBack(self,Opaque,aID,index,
                   GetPointerFromIndex(index,aTempData,@DataLen),DataLen) then
                  exit; // forced iteration break
              end;
            end;
          inc(next[stopper]); // find next fDeleted[] or fAlias*[]
          break; // we reached StopID -> get next udpated or deleted ID
        end else
        if DontRetrieveData then begin
          if not aCallBack(self,Opaque,aID,0,nil,0) then
            exit; // forced iteration break
        end else
        if not aCallBack(self,Opaque,aID,i,
           GetPointerFromIndex(i,aTempData,@DataLen),DataLen) then
          exit; // forced iteration break
      end;
      end; // case Order of
    end;
    assert(j=n);
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.GetLength(aID: integer): integer;
var i: integer;
begin
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    if (self=nil) or (fFile=0) or (aID<=0) or
       (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0) then
      result := -1 else begin
      i := IDToIndex(aID);
      if i<0 then
        result := -1 else begin
        if i<fCount then
          // retrieve from file
          result := Offset[i]-Offset[i-1] else
          // retrieve from data not already written to disk
          result := length(fInMemoryData[i-fCount]);
      end;
    end;
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.GetOffset(Index: integer): Int64;
begin
  if (Self=nil) or (cardinal(Index)>=cardinal(fCount)) then
    result := 0 else // avoid GPF
    if fOffset64<>nil then
      result := fOffset64[Index] else
      result := PtrUInt(fOffset32[Index]);
end;

function TSynBigTable.GetPart(aID, Offset, Len: Integer;
  out aData: RawByteString): boolean;
var i: integer;
    OffsetBegin: Int64;
    FileLen: integer;
begin // note: caching should not be used here
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    result := false;
    if (self=nil) or (fFile=0) or (aID<=0) or
       (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0) then
      exit;
    i := IDToIndex(aID);
    if i<0 then
      exit;
    if i<fCount then begin
      // retrieve from file
      OffsetBegin := GetOffset(i-1);
      FileLen := Int64(GetOffset(i)-OffsetBegin);
      if Offset>=FileLen then
        exit;
      inc(OffsetBegin,PtrUInt(Offset));
      fReadBuffer.Seek(OffsetBegin);
      if Offset+Len>FileLen then
        Len := FileLen-Offset; // truncate to real data size
      SetLength(aData,Len);
      fReadBuffer.Read(pointer(aData),Len);
    end else begin
      // retrieve from data not already written to disk
      FileLen := length(fInMemoryData[i-fCount]);
      if Offset>=FileLen then
        exit;
      if Offset+Len>FileLen then
        Len := FileLen-Offset; // truncate to real data size
      aData := Copy(fInMemoryData[i-fCount],Offset+1,Len);
    end;
    result := true;
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.GetPointer(aID: Integer;
  var aTempData: RawByteString; DataLen: PInteger): pointer;
begin
{$ifdef THREADSAFE}
  fLock.BeginRead;
  try
{$endif}
    if (self=nil) or (fFile=0) or (aID<=0) or
       (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0) then
      result := nil else
      result := GetPointerFromIndex(IDToIndex(aID),aTempData,DataLen);
{$ifdef THREADSAFE}
  finally
    fLock.EndRead;
  end;
{$endif}
end;

function TSynBigTable.GetPointerFromIndex(aIndex: Integer;
  var aTempData: RawByteString; DataLen: PInteger): pointer;
var OffsetBegin: Int64;
    len: PtrInt;
begin
  if cardinal(aIndex)<cardinal(fCount) then begin
    if fOffset64<>nil then begin
      if aIndex=0 then
        OffsetBegin := 0 else
        OffsetBegin := fOffset64[aIndex-1];
      fReadBuffer.Seek(OffsetBegin);
      len := Int64(fOffset64[aIndex]-OffsetBegin);
    end else begin
      if aIndex=0 then
        len := 0 else
        len := fOffset32[aIndex-1];
      fReadBuffer.Seek(len);
      len := fOffset32[aIndex]-len;
    end;
    result := fReadBuffer.ReadPointer(len,aTempData);
    if DataLen<>nil then
      DataLen^ := len;
  end else begin
    dec(aIndex,fCount);
    if cardinal(aIndex)<cardinal(fInMemoryCount) then begin
      result := pointer(fInMemoryData[aIndex]);
      if DataLen<>nil then
        DataLen^ := length(AnsiString(result));
    end else
      result := nil;
  end;
end;

function TSynBigTable.GetPointerFromPhysicalIndex(aPhysicalIndex: integer;
  var aTempData: RawByteString): pointer;
var OffsetBegin: Int64;
    Offs32: PtrInt;
begin
  if cardinal(aPhysicalIndex)<cardinal(fCount) then
  if fOffset64<>nil then begin
    if aPhysicalIndex=0 then
      OffsetBegin := 0 else
      OffsetBegin := fOffset64[aPhysicalIndex-1];
    fReadBuffer.Seek(OffsetBegin);
    result := fReadBuffer.ReadPointer(Int64(fOffset64[aPhysicalIndex]-OffsetBegin),aTempData);
  end else begin
    if aPhysicalIndex=0 then
      Offs32 := 0 else
      Offs32 := fOffset32[aPhysicalIndex-1];
    fReadBuffer.Seek(Offs32);
    result := fReadBuffer.ReadPointer(fOffset32[aPhysicalIndex]-Offs32,aTempData);
  end else begin
    dec(aPhysicalIndex,fCount);
    if cardinal(aPhysicalIndex)<cardinal(fInMemoryCount) then
      result := pointer(fInMemoryData[aPhysicalIndex]) else
      result := nil;
  end;
end;

function TSynBigTable.IDToIndex(aID: integer; handleAliases: boolean=true): integer;
begin
  if (Self=nil) or (aID<=0) or (aID>fCurrentID) then
    result := -1 else begin
    if handleAliases and (fAliasCount>0) then begin
      result := FastFindIntegerSorted(pointer(fAliasSource),fAliasCount-1,aID);
      if result>=0 then
        aID := fAliasReal[result]; // get updated ID
    end;
    result := FastFindIntegerSorted(pointer(fInMemoryID),fInMemoryCount-1,aID);
    if result<0 then
      result := FastFindIntegerSorted(pointer(fID),fCount-1,aID) else
      inc(result,fCount);
  end;
end;

function TSynBigTable.IndexToID(aIndex: Integer; handleAliases: boolean): integer;
begin
  result := 0;
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount+fInMemoryCount)) then
    exit;
  if aIndex<fCount then
    result := fID[aIndex] else
    result := fInMemoryID[aIndex-fCount];
  if not handleAliases then
    exit;
  aIndex := IntegerScanIndex(pointer(fAliasReal),fAliasCount,result);
  if aIndex>=0 then
    result := fAliasSource[aIndex];
end;

function TSynBigTable.InternalIDToIndex(aID: integer;
  var aAliasIndex: integer): integer;
begin
  if (self=nil) or (aID>fCurrentID) or (aID<=0) or
     (FastFindIntegerSorted(pointer(fDeleted),fDeletedCount-1,aID)>=0) then
    result := -1 else begin
    aAliasIndex := FastFindIntegerSorted(pointer(fAliasSource),fAliasCount-1,aID);
    if aAliasIndex>=0 then
      result := IDToIndex(fAliasReal[aAliasIndex],false) else
      result := IDToIndex(aID,false);
  end;
end;

class function TSynBigTable.InternalMagic: integer;
begin
  result := integer($ABAB0006);
end;

function TSynBigTable.InternalUpdateFromIndex(const aData: RawByteString;
  aID, aIndex, aAliasIndex: integer; var aNewIndex: cardinal): integer;
var i: integer;
begin
  assert(aIndex>=0);
  if aIndex>=fCount then begin
    // the data is still in-memory -> refresh in place (much faster)
    fInMemoryData[aIndex-fCount] := aData;
    fModified := true;
    aNewIndex := aIndex;
    result := aID;
  end else begin
    // data on disk -> create new invisible record with supplied data
    result := Add(aData,0,@aNewIndex,aIndex);
    if result>0 then
      if aAliasIndex>=0 then begin
        // already updated -> delete previous content, and assign new one
        AddSortedInteger(fDeleted,fDeletedCount,fAliasReal[aAliasIndex]);
        fAliasReal[aAliasIndex] := result;
        if fAliasRealSorted<>nil then
          SetLength(fAliasRealSorted,0);
      end else begin
        // add alias to this new content
        i := AddSortedInteger(fAliasSource,fAliasCount,aID,@fAliasReal);
        assert(i>=0); // we already checked not in fAliasSource[]
        fAliasReal[i] := result;
        if fAliasRealSorted<>nil then
          SetLength(fAliasRealSorted,0);
      end;
  end;
end;

class function TSynBigTable.FileFormatCheck(const aFileName: TFileName): boolean;
var magic: integer;
begin
  try
    with TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone) do
    try // same exact layout as in TSynBigTable.LoadFromFile
      result := (Seek(-4,soFromEnd)>0) and (Read(magic,4)=4) and
        (magic=InternalMagic);
    finally
      Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TSynBigTable.IterateGetAllID(Sender: TObject; Opaque: pointer;
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
var IDs: PIterateGetDynArrayIntegerOpaque absolute Opaque;
begin
  AddInteger(IDs.Values^,IDs.Count,ID);
  result := true;
end;

function TSynBigTable.IteratePack(Sender: TObject; Opaque: pointer; ID,
  Index: integer; Data: pointer; DataLen: integer): boolean;
var Added: PUpdateFieldEvent absolute Opaque;
begin
  with Added^ do begin
    result := Count<length(IDs);
    if result then begin
      WR.Write(Data,DataLen);
      Offsets64[Count] := WR.TotalWritten;
      IDs[Count] := ID;
      NewIndexs[Index] := Count;
      inc(Count);
    end;
  end;
end;

procedure TSynBigTable.LoadFromFile;
var magic: integer;
    CustomHeaderSize: integer;
    UseOffset64: boolean;
begin
  // reinitialize all internal structures
  if pointer(fInMemoryData)<>nil then
    Finalize(fInMemoryData);
  if fAliasRealSorted<>nil then
    SetLength(fAliasRealSorted,0);
  if fOffset64<>nil then
    SetLength(fOffset64,0);
  if fOffset32<>nil then
    SetLength(fOffset32,0);
  fCurrentID := 0;
  fInMemoryCount := 0;
  fCurrentInMemoryDataSize := 0;
  fCount := 0;
  fAliasCount := 0;
  fDeletedCount := 0;
  // open existing file
  fFile := FileOpen(fFileName,fmOpenReadWrite or fFileOpenMode);
  if (fFile<>INVALID_HANDLE_VALUE) and (FileSeek64(fFile,0,soFromEnd)<16) then begin
    FileClose(fFile); // size if not big enough -> force create new
    fFile := INVALID_HANDLE_VALUE;
  end;
  if fFile=INVALID_HANDLE_VALUE then begin
    fFile := FileCreate(fFileName);
    if (fFileOpenMode<>0) and (fFile<>INVALID_HANDLE_VALUE) then begin
      FileClose(fFile);
      fFile := FileOpen(fFileName,fmOpenReadWrite or fFileOpenMode);
    end;
    CustomHeader(sbtAfterRead,nil); // refresh any internal index e.g.
    exit; // no data yet
  end;
  // load headers from existing file
  fReadBuffer.Open(fFile);
  magic := 0;
  if fReadBuffer.Seek(fReadBuffer.FileSize-8) then begin
    fReadBuffer.Read(@CustomHeaderSize,4);
    fReadBuffer.Read(@magic,4);
  end;
  if (magic<>InternalMagic) or (CustomHeaderSize<=0) then
    CustomHeaderSize := 0 else begin
    if not fReadBuffer.Seek(fReadBuffer.FileSize-CustomHeaderSize) then
      CustomHeaderSize := 0 else begin
      fCount := fReadBuffer.ReadVarUInt32;
      fDeletedCount := fReadBuffer.ReadVarUInt32;
      fAliasCount := fReadBuffer.ReadVarUInt32;
      if (fCount<0) or (PtrUInt(fDeletedCount)>PtrUInt(fCount)) or
         (PtrUInt(fAliasCount)>PtrUInt(fCount)) then
        CustomHeaderSize := 0;
    end;
  end;
  if CustomHeaderSize>0 then begin // valid file content
    SetLength(fID,fCount);
    SetLength(fDeleted,fDeletedCount);
    CustomHeader(sbtRead,nil);
    if fCount=0 then begin
      CustomHeader(sbtAfterRead,nil); // refresh any internal index e.g.
      exit;
    end;
    with fReadBuffer do begin
      fCurrentID := ReadVarUInt32;
      if (fCurrentID<=0) or
         (ReadVarUInt32Array(fID)<>fCount) or
         (ReadVarUInt32Array(fDeleted)<>fDeletedCount) or
         (ReadVarUInt32Array(fAliasSource)<>fAliasCount) or
         (ReadVarUInt32Array(fAliasReal)<>fAliasCount) then
        CustomHeaderSize := 0 else begin
        Read(@UseOffset64,1);
        if (UseOffset64 and (ReadVarUInt64Array(fOffset64)<>fCount)) or
           (not UseOffset64 and (ReadVarUInt32Array(fOffset32)<>fCount)) then
          CustomHeaderSize := 0 else // invalid file content
          CustomHeader(sbtAfterRead,nil); // refresh any internal index e.g.
      end;
    end;
  end;
  if CustomHeaderSize>0 then
    exit;
  fReadBuffer.Close;
  FileClose(fFile);
  fFile := 0; // most class methods will check this value and fail
  fCount := 0;
  fDeletedCount := 0;
  fAliasCount := 0;
end;

procedure TSynBigTable.Pack(forceFlushOnDisk: boolean=false);
var i, ndx, Di,Si,Ni,nCount,oneBuf,firstDel,nNewIndexs: integer;
    S,D,Diff,L,N: Int64;
    buf: RawByteString;
    index, NewIndexs: TIntegerDynArray;
begin
  if (self=nil) or ((fDeletedCount=0) and (fAliasCount=0)) or (fCount=0) or 
     (fFile=0) then
    exit; // nothing to pack
{$ifdef THREADSAFE}
  fLock.BeginWrite;
  try
{$endif}
    if fAliasRealSorted<>nil then
      SetLength(fAliasRealSorted,0);
    if fAliasCount=0 then begin
      // 1. if no updated item, we can pack the file in-place (faster)
      if Assigned(OnAfterPack) then begin
        nNewIndexs := fCount+fInMemoryCount;
        SetLength(NewIndexs,nNewIndexs);
        FillIncreasing(pointer(NewIndexs),0,nNewIndexs);
      end;
      fReadBuffer.Close; // unmap all reading files
      // mark all deleted IDs as fID[]=0 - pack is faster if done first
      Setlength(index,fDeletedCount);
      Di := maxInt;
      firstDel := -1;
      for i := 0 to fDeletedCount-1 do
      if fDeleted[i]>0 then begin
        ndx := FastFindIntegerSorted(pointer(fID),fCount-1,fDeleted[i]);
        if ndx>=0 then begin // ignore fInMemory*[] blocks
          index[i] := ndx; // no direct fID[] := 0 because of fID is used above
          if ndx<Di then
            Di := ndx;
        end else begin
          firstDel := i; // first fDeleted[] index in fInMemory*[]
          break;
        end;
      end else
        assert(false);
      if firstDel<0 then // no deleted fInMemory*[] block -> no more delete
        fDeletedCount := 0 else begin
        dec(fDeletedCount,firstDel); // keep only fDeleted[] for fInMemory*[] blocks
        Move(fDeleted[firstDel],fDeleted[0],fDeletedCount*4);
      end;
      // update fID[] with deleted items
      for i := 0 to high(index) do
      if index[i]>0 then // index[i]=0 for fInMemory*[] deleted blocks
        fID[index[i]] := 0;
      // save packed file in-place (it's faster than with 2 files)
      if Di<maxInt then begin
        SetLength(buf,32*1024*1024); // 32 MB buffer should be big enough
        D := Offset[Di-1];
        nCount := Di;
        repeat
          Si := Di+1;
          while (Si<fCount) and (fID[Si]=0) do inc(Si); // Si = next not deleted entry
          if Si=fCount then
            break; // deleted till the end
          S := Offset[Si-1];
          Ni := Si+1;
          while (Ni<fCount) and (fID[Ni]<>0) do inc(Ni); // Ni= next deleted entry
          N := Offset[Ni-1]; // if no more deleted entry, Ni=fCount
          L := N-S;
          repeat // move whole not deleted data block
            OneBuf := length(buf);
            if L<OneBuf then
              OneBuf := L;
            FileSeek64(fFile,S,soFromBeginning);
            FileRead(fFile,pointer(buf)^,OneBuf);
            FileSeek64(fFile,D,soFromBeginning);
            FileWrite(fFile,pointer(buf)^,OneBuf);
            dec(L,OneBuf);
            inc(S,OneBuf);
            inc(D,OneBuf);
          until L=0;
          Diff := S-D; // whole deleted size
          for i := Si to Ni-1 do begin // add all not deleted blocks
            if fOffset64<>nil then
              fOffset64[nCount] := fOffset64[i]-Diff else
              fOffset32[nCount] := fOffset32[i]-PtrInt(Diff);
            if NewIndexs<>nil then
              NewIndexs[i] := nCount;
            fID[nCount] := fID[i];
            inc(nCount);
    //        assert(fID[i]<>0);
          end;
    //      assert(D=FileSeek64(fFile,0,soFromCurrent));
    //      assert(fOffset[nCount-1]=D);
          Di := Ni;
        until Di=fCount;
        fCount := nCount;
      end;
      if Assigned(OnAfterPack) then
        OnAfterPack(NewIndexs);
      fModified := true; // force save to file
      UpdateToFile(forceFlushOnDisk);
    end else begin
      // 2. some items were updated -> we'll need two files
      RecreateFileContent(IteratePack); // will do all the magic
    end;
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;

procedure TSynBigTable.RecreateFileContent(aCallBack: TSynBigTableIterateEvent;
  const OpaqueFields: TSQLFieldBits=[]);
var n: integer;
    aFileName: TFileName;
    Opaque: TUpdateFieldEvent;
begin
  if (self=nil) or not Assigned(aCallBack) then
    exit;
  n := Count;
  aFileName := fFileName+'.~new';
  DeleteFile(aFileName);
  // prepare opaque structure
  FillChar(Opaque,sizeof(Opaque),0);
  Opaque.AvailableFields := OpaqueFields;
  Opaque.WR := TFileBufferWriter.Create(aFileName,1 shl 24); // write per 16 MB blocks
  try
    // create new data
    SetLength(Opaque.IDs,n);
    SetLength(Opaque.Offsets64,n);
    SetLength(Opaque.NewIndexs,fCount+fInMemoryCount); // NewIndexs[old] := new
    GetIterating(aCallBack,ioID,@Opaque); // will do the work
    assert(Opaque.Count=n);
    Opaque.WR.Flush;
    // close previous file
    fReadBuffer.Close;
    FileClose(fFile);
    fFile := 0;
    // update internal structures with new file content
    if Assigned(OnAfterPack) then
      OnAfterPack(Opaque.NewIndexs); // not existing -> created by LoadFromFile
    fCount := n;
    fCurrentID := Opaque.IDs[n-1];
    fInMemoryCount := 0;
    fAliasCount := 0;
    fDeletedCount := 0;
    SetLength(fID,n);
    move(Opaque.IDs[0],fID[0],n*4);
    if (n>0) and (Opaque.Offsets64[n-1]>1 shl 30) then begin
      SetLength(fOffset64,n);
      SetLength(fOffset32,0);
      move(Opaque.Offsets64[0],fOffset64[0],n*8);
    end else begin
      SetLength(fOffset64,0);
      SetLength(fOffset32,n);
      Int64ToUInt32(pointer(Opaque.Offsets64),pointer(fOffset32),n);
    end;
    // add headers to the new file
    if not DeleteFile(fFileName) then begin
      Sleep(1000);
      DeleteFile(fFileName);
    end;
    fFile := THandleStream(Opaque.WR.Stream).Handle; // write on the new file
    fModified := true;
    UpdateToFile(true,true); // flush + don't reopen fReadBuffer now
  finally
    Opaque.WR.Free;
  end;
  // use the new file
  MoveFile(Pointer(aFileName),Pointer(fFileName));
  LoadFromFile;
end;

function TSynBigTable.TestIterateEvent(Sender: TObject; Opaque: pointer;
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
var len: integer;
    aTempData: RawByteString;
begin // in some cases, Data can be nil (some data string='')
  result := true;
  if (Sender<>self) or not Get(ID,aTempData) or (Opaque=nil) then
    exit; // use Get() for testing GetPointer() returned value
  len := length(aTempData);
  if (len<>DataLen) or not CompareMem(pointer(aTempData),Data,len) then
  if (len<>DataLen) or not Get(ID,aTempData) or not CompareMem(pointer(aTempData),Data,len) then
    exit; // slow but proven comparison
  inc(PInteger(Opaque)^); // will fail test if not reached this line
end;

function TSynBigTable.TestIterateEventSpeed(Sender: TObject; Opaque: pointer; 
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
begin
  result := true; // void call-back to test GetIterating() speed
end;

function TSynBigTable.Update(aID: Integer; const aData: RawByteString;
  PhysicalIndexOldNew: PInt64=nil): integer;
var ndx, aliasNdx: integer;
    OldNew: Int64Rec; // Lo=Old Hi=New
begin
{$ifdef THREADSAFE}
  fLock.BeginWrite;
  try
{$endif}
    ndx := InternalIDToIndex(aID,aliasNdx);
    if ndx<0 then // invalid ID
      result := 0 else begin
      result := InternalUpdateFromIndex(aData,aID,ndx,aliasNdx,OldNew.Hi);
      if PhysicalIndexOldNew<>nil then begin
        OldNew.Lo := ndx; // Lo=Old
        PhysicalIndexOldNew^ := Int64(OldNew);
      end;
    end;
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;

procedure TSynBigTable.UpdateToFile(forceFlushOnDisk: boolean=false;
  dontReopenReadBuffer: boolean=false);
var i,delNdx: integer;
    magic: integer;
    Offs: Int64;
    UseOffset64: boolean;
    Len: PtrUInt;
    WriteBuffer: TFileBufferWriter;
begin
  if (fFile=0) or not fModified then
    exit; // nothing to write
{$ifdef THREADSAFE}
  fLock.BeginWrite;
  try
{$endif}
    // prepare the internal structures for writing
    CustomHeader(sbtBeforeWrite,nil); // refresh any internal index e.g.
    // now update file content
    fReadBuffer.Close;
    Offs := Offset[fCount-1];
    FileSeek64(fFile,Offs,soFromBeginning);
    WriteBuffer := TFileBufferWriter.Create(fFile);
    try
      // append new blocks of data, and update fOffset*[] and fID[]
      if fInMemoryCount>0 then begin // fInMemoryCount=-1 from Pack call
        if Offs+CurrentInMemoryDataSize>1 shl 30 then
          // global file size will be > 1 GB -> use fOffset64[]
          if fOffset64=nil then begin
            SetLength(fOffset64,fCount+fInMemoryCount);
            for i := 0 to fCount-1 do
              fOffset64[i] := PtrUInt(fOffset32[i]);
            SetLength(fOffset32,0);
          end else
            SetLength(fOffset64,fCount+fInMemoryCount) else
          // total size < 1 GB -> less memory and faster with fOffset32[]  
          SetLength(fOffset32,fCount+fInMemoryCount);
        SetLength(fID,fCount+fInMemoryCount);
        for i := 0 to fInMemoryCount-1 do begin
          delNdx := FastFindIntegerSorted(
            pointer(fDeleted),fDeletedCount-1,fInMemoryID[i]);
          if delNdx>=0 then
            DeleteInteger(fDeleted,fDeletedCount,delNdx) else begin
            Len := length(fInMemoryData[i]);
            WriteBuffer.Write(Pointer(fInMemoryData[i]),Len);
            inc(Offs,Len);
            if fOffset64<>nil then
              fOffset64[fCount] := Offs else
              fOffset32[fCount] := Offs;
            fID[fCount] := fInMemoryID[i];
            inc(fCount);
          end;
        end;
      end;
      // reset fInMemory*[] content
      Finalize(fInMemoryData);
      Finalize(fInMemoryID);
      fInMemoryCount := 0;
      fCurrentInMemoryDataSize := 0;
      fModified := false;
      // rewrite header
      Offs := WriteBuffer.TotalWritten;
      magic := InternalMagic;
      WriteBuffer.WriteVarUInt32(fCount);
      WriteBuffer.WriteVarUInt32(fDeletedCount);
      WriteBuffer.WriteVarUInt32(fAliasCount);
      CustomHeader(sbtWrite,WriteBuffer);
      WriteBuffer.WriteVarUInt32(fCurrentID);
      WriteBuffer.WriteVarUInt32Array(fID,fCount,wkSorted);
      WriteBuffer.WriteVarUInt32Array(fDeleted,fDeletedCount,wkSorted);
      WriteBuffer.WriteVarUInt32Array(fAliasSource,fAliasCount,wkSorted);
      WriteBuffer.WriteVarUInt32Array(fAliasReal,fAliasCount,wkVarUInt32);
      UseOffset64 := pointer(fOffset64)<>nil;
      WriteBuffer.Write(@UseOffset64,1);
      if UseOffset64 then
        WriteBuffer.WriteVarUInt64DynArray(fOffset64,fCount,true) else
        WriteBuffer.WriteVarUInt32Array(fOffset32,fCount,wkOffsetU);
      Offs := WriteBuffer.TotalWritten-Offs+8;
      //assert(Offs<maxInt);
      WriteBuffer.Write(@Offs,4);
      WriteBuffer.Write(@magic,4);
      WriteBuffer.Flush;
    finally
      WriteBuffer.Free;
    end;
    SetEndOfFile(fFile); // always force truncate at end of header
    if forceFlushOnDisk then
      FlushFileBuffers(fFile);
    if not dontReopenReadBuffer then
      fReadBuffer.Open(fFile);
{$ifdef THREADSAFE}
  finally
    fLock.EndWrite;
  end;
{$endif}
end;


{ TSynBigTableString }

function TSynBigTableString.Add(const aData: RawByteString; const aID: RawUTF8;
  ForcedID: integer=0): integer;
var i: integer;
begin
  if (aID='') or (self=nil) then
    result := 0 else begin
    i := FastLocatePUTF8CharSorted(pointer(fHeaderString),fHeaderCount-1,pointer(aID));
    if i<0 then // aID already exists -> fails
      result := 0 else begin
      result := inherited Add(aData,ForcedID);
      if result>0 then begin
        AddSortedRawUTF8(fHeaderString,fHeaderCount,aID,@fHeaderID,i);
        fHeaderID[i] := result;
      end;
    end;
  end;
end;

procedure TSynBigTableString.Clear;
begin
  inherited Clear;
  Finalize(fHeaderString[0],fHeaderCount); // force set all String ID to ''
  fHeaderCount := 0;
end;

function TSynBigTableString.CustomHeader(Action: TSynBigTableCustomHeader;
  WriteBuffer: TFileBufferWriter): integer;
begin
  result := 0; // make compiler happy
  case Action of
    sbtWrite: begin
      WriteBuffer.WriteVarUInt32Array(fHeaderID,fHeaderCount,wkVarUInt32);
      WriteBuffer.WriteRawUTF8DynArray(fHeaderString,fHeaderCount);
    end;
    sbtRead: begin
      Finalize(fHeaderString); // force set all String ID to ''
      fHeaderCount := fReadBuffer.ReadVarUInt32Array(fHeaderID);
      if (fHeaderCount<>fCount-fDeletedCount-fAliasCount) or
         (fHeaderCount<>fReadBuffer.ReadVarRawUTF8DynArray(fHeaderString)) then
        fReadBuffer.ErrorInvalidContent;
    end;
  end;
end;

function TSynBigTableString.Delete(aID: integer; PhysicalIndex: PInteger): boolean;
var ndx: integer;
begin
  result := inherited Delete(aID);
  if not result then
    exit;
  ndx := IntegerScanIndex(pointer(fHeaderID),fHeaderCount,aID);
  if ndx>=0 then
    result := DeleteRawUTF8(fHeaderString,fHeaderCount,ndx,@fHeaderID);
end;

function TSynBigTableString.Delete(const aID: RawUTF8): boolean;
var ID: integer;
begin
  ID := StringToID(aID);
  if ID=0 then
    result := false else
    result := Delete(ID);
end;

function TSynBigTableString.Get(const aID: RawUTF8; out aData: RawByteString): boolean;
var ID: integer;
begin
  ID := StringToID(aID);
  if ID=0 then
    result := false else
    result := Get(ID,aData);
end;

function TSynBigTableString.GetAllIDs(var IDs: TIntegerDynArray;
  Order: TSynBigTableIterationOrder): integer;
begin
  case Order of
  ioPhysical:
    // physical order is achieved by GetIterating()
    inherited GetAllIDs(IDs,ioPhysical);
  ioID:
    // faster in-place quick sort than GetIterating() call
    CopyAndSortInteger(pointer(fHeaderID),fHeaderCount,IDs);
  ioFaster: begin
    // no quicksort of IDs (leave string key order)
    if fHeaderCount>length(IDs) then
      SetLength(IDs,fHeaderCount);
    Move(fHeaderID[0],IDs[0],fHeaderCount*sizeof(integer));
  end;
  end;
  result := fHeaderCount;
end;

function TSynBigTableString.GetAsStream(const aID: RawUTF8): TStream;
var ID: integer;
begin
  ID := StringToID(aID);
  if ID=0 then
    result := nil else
    result := GetAsStream(ID);
end;

function TSynBigTableString.GetCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fHeaderCount;
end;

function TSynBigTableString.GetID(Index: integer): integer;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fHeaderCount)) then
    result := 0 else
    result := fHeaderID[Index];
end;

function TSynBigTableString.GetPointer(const aID: RawUTF8;
  var aTempData: RawByteString; DataLen: PInteger): pointer;
var ID: integer;
begin
  ID := StringToID(aID);
  if ID=0 then
    result := nil else
    result := GetPointer(ID,aTempData,DataLen);
end;

function TSynBigTableString.GetStringID(Index: integer): RawUTF8;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fHeaderCount)) then
    result := '' else
    result := fHeaderString[Index];
end;

function TSynBigTableString.IDToString(aID: integer): RawUTF8;
begin
  if (aID<=0) or (self=nil) or (fFile=0) or (fHeaderCount=0) then
    result := '' else begin
    aID := IntegerScanIndex(pointer(fHeaderID),fHeaderCount,aID);
    if aID<0 then
      result := '' else
      result := fHeaderString[aID];
  end;
end;

class function TSynBigTableString.InternalMagic: integer;
begin
  result := integer($ABAB0007); // differs from TSynBigTable
end;

function TSynBigTableString.StringToID(const aID: RawUTF8): integer;
begin // use fast binary search
  if (aID='') or (self=nil) or (fFile=0) then
    result := 0 else begin
    result := FastFindPUTF8CharSorted(pointer(fHeaderString),fHeaderCount-1,pointer(aID));
    if result<0 then
      result := 0 else
      result := fHeaderID[result];
  end;
end;

function TSynBigTableString.Update(const aData: RawByteString;
  const aID: RawUTF8): boolean;
var ID: integer;
begin
  ID := StringToID(aID);
  if ID=0 then
    result := false else
    result := Update(ID,aData)>0;
end;


{ TSynBigTableRecord }

function TSynBigTableRecord.Add(const aData: RawByteString; ForcedID: integer;
  PhysicalIndex: PInteger; OldPhysicalIndex: integer): integer;
var ndx: integer;
    aTemp: RawByteString;
begin
  if CheckConstraints(aData,OldPhysicalIndex) then begin
    Result := inherited Add(aData,ForcedID,@ndx,OldPhysicalIndex);
    if Result<>0 then begin
      if PhysicalIndex<>nil then
        PhysicalIndex^ := ndx;
      Table.FieldIndexModify(OldPhysicalIndex,ndx,
          GetPointerFromIndex(OldPhysicalIndex,aTemp),pointer(aData));
    end;
  end else
    Result := 0;
end;

procedure TSynBigTableRecord.AddFieldUpdate;
var F: integer;
    Fields: TSQLFieldBits;
begin
  if (self=nil) or (Table.AddedField=nil) then
    exit; // nothing new
  if Count=0 then begin
    // no data to refresh
    Table.AddedField.Free; // do it once
    Table.AddedField := nil;
  end else begin
    // some data to refresh: guess field added, and process
    fillchar(Fields,sizeof(Fields),0);
    for F := 0 to Table.FieldCount-1 do
      if Table.AddedField.IndexOf(Table.Field[F])<0 then
        Include(Fields,F);
    Table.AddedField.Free; // do it once
    Table.AddedField := nil;
    RecreateFileContent(Table.UpdateFieldEvent,Fields);
  end;
end;

constructor TSynBigTableRecord.Create(const aFileName: TFileName;
  const aTableName: RawUTF8; FileOpenMode: Cardinal);
begin
  inherited Create(aFileName,aTableName,GetPointerFromPhysicalIndex,FileOpenMode);
  OnAfterPack := RefreshIndexes;
end;

function TSynBigTableRecord.CustomHeader(Action: TSynBigTableCustomHeader;
  WriteBuffer: TFileBufferWriter): integer;
var F,n: integer;
    PhysicalIndexes: TIntegerDynArray;
begin
  result := 0; // make compiler happy
  case Action of
    sbtBeforeWrite:
      for F := 0 to Table.FieldCount-1 do
        Table.Field[F].OrderedIndexRefresh;
    sbtWrite:
      if WriteBuffer<>nil then
        Table.SaveTo(WriteBuffer);
    sbtRead: begin
      Table.LoadFrom(fReadBuffer);
      fTableName := Table.TableName;
    end;
    sbtAfterRead: begin
      n := Count;
      if n>0 then
      for F := 0 to Table.FieldCount-1 do
        with Table.Field[F] do
        if tfoIndex in Options then
          if (OrderedIndexCount<>n) or (length(OrderedIndex)<n) then begin
            // this index need to be recreated -> do it now
            OrderedIndexCount := n;
            if PhysicalIndexes=nil then
              GetAllPhysicalIndexes(PhysicalIndexes);
            SetLength(OrderedIndex,n);
            Move(PhysicalIndexes[0],OrderedIndex[0],n*4);
            OrderedIndexUpdate(-1,-1,nil,nil); // both indexes equal -1 -> just sort
            fModified := true;
          end;
    end;
  end;
end;

function TSynBigTableRecord.Delete(aID: integer; PhysicalIndex: PInteger): boolean;
var ndx: integer;
begin
  Result := inherited Delete(aID,@ndx);
  if Result then begin
    if PhysicalIndex<>nil then
      PhysicalIndex^ := ndx;
    Table.FieldIndexModify(ndx,-1,nil,nil);
  end;
end;

class function TSynBigTableRecord.InternalMagic: integer;
begin
  result := integer($A5A50001); // genuine ID which differs from others
end;

function TSynBigTableRecord.RecordAdd(const aRecord: TSynTableData; aForcedID: integer): integer;
begin
  if aRecord.SBF='' then
    result := 0 else
    result := Add(aRecord.SBF,aForcedID);
end;

procedure TSynBigTableRecord.RecordGet(aID: integer; var result: TSynTableData);
var Temp: RawByteString;
    RecordBufLen: integer;
begin
  result.Init(Table,aID,GetPointer(aID,Temp,@RecordBufLen),RecordBufLen);
end;

function TSynBigTableRecord.RecordUpdate(const aRecord: TSynTableData): boolean;
begin
  result := Update(aRecord.ID,aRecord.SBF)<>0;
end;

procedure TSynBigTableRecord.RefreshIndexes(var NewIndexs: TIntegerDynArray);
var n, i, F: integer;
begin
  for n := 0 to high(NewIndexs) do
    if NewIndexs[n]<>n then begin
      // at least one physical index changed -> update OrderedIndex[] array
      for F := 0 to Table.FieldCount-1 do
        with Table.Field[F] do
        if OrderedIndex<>nil then
          for i := 0 to OrderedIndexCount-1 do
            OrderedIndex[i] := NewIndexs[OrderedIndex[i]];
      exit;
    end;
end;

procedure TSynBigTableRecord.SearchIterating(aCallBack: TSynBigTableIterateEvent;
  Order: TSynBigTableIterationOrder; Opaque: pointer);
begin
  GetIterating(aCallBack,Order,Opaque);
end;

function TSynBigTableRecord.Update(aID: Integer;
  const aData: RawByteString; PhysicalIndexOldNew: PInt64): integer;
var newNdx: cardinal;
    ndx, aliasNdx: integer;
begin
  ndx := InternalIDToIndex(aID,aliasNdx);
  if (ndx<0) or not CheckConstraints(aData,ndx) then // invalid ID or data
    result := 0 else begin
    result := InternalUpdateFromIndex(aData,aID,ndx,aliasNdx,newNdx);
    if ndx>=fCount then // was refreshed in memory -> manual index refresh
      Table.FieldIndexModify(ndx,newNdx,nil,nil);
      // old data was overriden -> no GetPointerFromIndex(ndx,aTemp),pointer(aData));
     // aIndex<fCount -> TSynBigTableRecord.Add() did call FieldIndexModify
  end;
end;

{$ifndef LVCL}
function TSynBigTableRecord.VariantAdd(const aRecord: Variant): integer;
var Data: RawByteString;
begin
  Data := TSynTableVariantType.ToSBF(aRecord);
  if Data='' then
    result := 0 else
    result := Add(Data);
end;

function TSynBigTableRecord.VariantGet(aID: integer): Variant;
var Temp: RawByteString;
    RecordBuf: pointer;
begin
  RecordBuf := GetPointer(aID,Temp);
  if RecordBuf=nil then
    VarClear(result) else
    result := fTable.Data(aID,RecordBuf);
end;

function TSynBigTableRecord.VariantUpdate(const aRecord: Variant): boolean;
begin
  with TSynTableVariantType do
    result := Update(ToID(aRecord),ToSBF(aRecord))<>0;
end;
{$endif LVCL}



{ TTestBigTable }

class function TTestBigTable.CreateString(aID, n: integer): RawByteString;
var i: integer;
begin
  result := '';
  if n=0 then
    n := Random(1000);
  SetLength(result,n*4); // each data block size is up to 4 KB
  for i := 0 to n-1 do
    PIntegerArray(result)^[i] := aID;
end;

function TTestBigTable.Delete(var T: TSynBigTable; n: integer; Pack, withString: boolean): boolean;
var i,j,nu,id,tot: integer;
    St: TStream;
    tmp: TIntegerDynArray;
    ok: boolean;
    Data: RawByteString;
    TS: TSynBigTableString absolute T; // absolute T -> Delete() will work
begin
  result := false;
  SetLength(Deleted,Random(10)+10);
  fillchar(Deleted[0],length(Deleted)*4,0);
  for i := 0 to high(Deleted) do
  repeat
    repeat
      id := Random(n)+1;
    until (T.IDToIndex(id)>=0) and
      (IntegerScanIndex(pointer(Deleted),length(Deleted),id)<0); // no dup
    Deleted[i] := id;
    ok := T.Delete(id);
  until ok;
  for i := 0 to high(Deleted) do
    Check(FastFindIntegerSorted(pointer(T.fDeleted),T.fDeletedCount-1,Deleted[i])>=0);
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
  Check(tot=T.Count);
  if Pack then begin
    T.Pack; // delete data from disk
  end else begin
    T.Free; // verify after read than fDelete[] are saved
    if withString then
      TS := TSynBigTableString.Create(FN) else
      T := TSynBigTable.Create(FN);
  end;
  for i := 0 to high(Deleted) do
    if CheckFailed(not T.Get(Deleted[i],Data)) then
      exit;
  if Pack then begin
    for i := 1 to n do
      if IntegerScanIndex(pointer(Deleted),length(Deleted),i)<0 then
      if T.Get(i,Data) then 
        if CheckFailed(TestString(i,Data)) then
          exit else
        if withString then
        if CheckFailed(TS.Get(Int32ToUTF8(i),Data)) or CheckFailed(TestString(i,Data)) then
          exit;
  end else
    for i := 1 to n do 
      if T.Get(i,Data) THEN
        if CheckFailed(TestString(i,Data)) then
          exit else
        if withString then
        if CheckFailed(TS.Get(Int32ToUTF8(i),Data)) or CheckFailed(TestString(i,Data)) then
          exit;
  nu := Random(100)+10;
  for i := 0 to nu-1 do
  repeat
    repeat
      id := Random(n)+1;
    until (T.IDToIndex(id)>=0) and
      (FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,id)<0); // no dup
    AddSortedInteger(Updated,UpdatedCount,id);
    T.Get(id,Data);
    UniqueString(AnsiString(Data));
    for j := 0 to (length(Data) shr 2)-1 do
      inc(PIntegerArray(Data)^[j],$01010101);
    ok := T.Update(id,Data)<>0;
  until ok;
  for i := 1 to n do
    if T.Get(i,Data) then
      if CheckFailed(TestString(i,Data)) then
        exit;
  for i := 1 to n do begin
    St := T.GetAsStream(i);
    if (St<>nil) and CheckFailed(TestStream(i,St)) then
      exit;
  end;
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioID,@tot);
  Check(tot=T.Count,'Iteration test ID order');
  tot := 0;
  T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
  Check(tot=T.Count,'Iteration test physical order');
  for i := 0 to T.Count-1 do
    Check(T.NumericalID[i]<>0);
  Check(T.GetAllIDs(tmp,ioPhysical)=T.Count,'GetAllIDs physical order');
  Check(T.GetAllIDs(tmp,ioID)=T.Count,'GetAllIDs ID order');
  Check(T.GetAllPhysicalIndexes(tmp)=T.Count,'GetAllPhysicalIndexes');
  result := true;
end;

function TTestBigTable.DoFieldTest(DoRecord: boolean; n: integer): boolean;
var T: TSynBigTableTable;
    TRec: TSynBigTableRecord absolute T;
    TMeta: TSynBigTableMetaData absolute T;
    By8: TRawUTF8DynArray;
function TRTest: boolean;
var i: integer;
{$ifndef LVCL}
    vari: variant;
    rec: TSynTableData;
{$endif}
    fText, fInt: TSynTableFieldProperties;
    ID: TIntegerDynArray;
    IDCount: integer;
    p: pointer;
    s: RawByteString;
begin
  result := false;
  fText := T.Table['Text'];
  fInt := T.Table['INT'];
{$ifndef LVCL}
  for i := 1 to n do begin
    vari := T.VariantGet(i);
    if CheckFailed(vari.text=By8[n-i]) or CheckFailed(vari.ID=i) or
       CheckFailed(vari.Int=i-1,'Read as Variant') then exit;
  end;
  for i := 1 to n do begin
    T.RecordGet(i,rec);
    if CheckFailed(RawUTF8(rec.GetFieldValue(fText))=By8[n-i]) or
       CheckFailed(integer(rec.GetFieldValue(fInt))=i-1,'Read as TSynTableData') then
      exit;
  end;
{$endif}
  for i := 1 to n do begin
    if DoRecord then
      p := TRec.GetPointer(i,s) else
      p := TMeta.GetMetaDataFromID(i);
    if CheckFailed(fInt.GetInteger(p)=i-1) or
       CheckFailed(fText.GetRawUTF8(p)=By8[n-i],'Read direct') then
      exit;
  end;
  IDCount := 0; // not mandatory (if ID=nil, T.Search will do it)
  for i := n-50 to n-1 do
    if CheckFailed(T.Search(fText,fText.SBF(By8[n-i]),ID,IDCount,0,ioPhysical)) or
       CheckFailed(IDCount=1) or CheckFailed(ID[0]=i,'Search 50 Text iterating') then
        exit else
        IDCount := 0;
  for i := n-200 to n-1 do
    if CheckFailed(T.Search(fText,fText.SBF(By8[n-i]),ID,IDCount)) or
      CheckFailed(IDCount=1) or CheckFailed(ID[0]=i,'Search 200 Text using index') then
        exit else
        IDCount := 0;
  for i := n-50 to n-1 do
    if CheckFailed(T.Search(fInt,fInt.SBF(i),ID,IDCount,0,ioPhysical)) or
       CheckFailed(IDCount=1) or CheckFailed(ID[0]=i+1,'Search 50 Int iterating') then
        exit else
        IDCount := 0;
  for i := n-200 to n-1 do
    if CheckFailed(T.Search(fInt,fInt.SBF(i),ID,IDCount)) or
      CheckFailed(IDCount=1) or CheckFailed(ID[0]=i+1,'Search 200 Int using index') then
        exit else
        IDCount := 0;
  if not DoRecord then
    for i := 1 to n do
      if CheckFailed(TMeta.Get(i,s)) or CheckFailed(TestString(i,s)) then
        exit;
  Result := true;
end;
var fText, fInt, fBool: TSynTableFieldProperties;
    i, aID, Value: integer;
    IDs: TIntegerDynArray;
    IDCount: integer;
    ClassName: string;
    rec: TSynTableData;
procedure SetS(var s: RawUTF8; i: cardinal);
var p: array[0..3] of Word;
begin
  p[2] := TwoDigitLookupW[(i div 100)mod 100];
  p[3] := TwoDigitLookupW[i mod 100];
  i := i div 10000;
  p[0] := TwoDigitLookupW[i div 100];
  p[1] := TwoDigitLookupW[i mod 100];
  SetString(s,PAnsiChar(@p[0]),8);
end;
const nInt=300; // unique field update is time consumming
begin
  result := false;
  if DoRecord then
    ClassName := TSynBigTableRecord.ClassName else
    ClassName := TSynBigTableMetaData.ClassName;
  FN := ChangeFileExt(FN,'.'+copy(ClassName,13,100));
  DeleteFile(FN);
  SetLength(By8,n);
  UpdatedCount := 0;
  for i := 1 to n do
    SetS(By8[i-1],i);
  if DoRecord then
    T := TSynBigTableRecord.Create(FN,'test') else
    T := TSynBigTableMetaData.Create(FN,'test');
  try
    T.AddField('text',tftWinAnsi,[tfoIndex]);
    T.AddField('Int',tftInt32,[tfoIndex,tfoUnique]);
    T.AddFieldUpdate;
    fText := T.Table['text']; // need to get it after AddFieldUpdate
    fInt := T.Table['int'];
    rec.Init(T.Table);
    for i := 0 to n-1 do begin
      rec.SetFieldSBFValue(fText,fText.SBF(By8[n-i-1])); 
      rec.SetFieldSBFValue(fInt,fInt.SBF(i));
      if DoRecord then
        Check(TRec.RecordAdd(rec)<>0) else
        Check(TMeta.RecordAdd(CreateString(i+1),rec)<>0);
    end;
    if tfoUnique in fInt.Options then
      for i := 0 to (n shr 3)-1 do begin
        rec.SetFieldSBFValue(fInt,fInt.SBF(i shl 3));
        if DoRecord then
          Check(TRec.RecordAdd(rec)=0) else
          Check(TMeta.RecordAdd(By8[i],rec)=0);
      end;
    if CheckFailed(TRTest) then exit;
    T.UpdateToFile;
    T.Free;
    if DoRecord then
      T := TSynBigTableRecord.Create(FN,'test') else
      T := TSynBigTableMetaData.Create(FN,'test');
    if CheckFailed(TRTest) then exit;
    Check(T.AddField('bool',tftBoolean));
    T.AddFieldUpdate;
    if CheckFailed(TRTest) then exit;
    fBool := TRec.Table['bool']; // need to get it after any AddFieldUpdate
    Check(fBool<>nil);
    for i := 0 to (n shr 3)-1 do begin
      aID := i shl 3+1;
      rec := T.RecordGet(aID);
      if CheckFailed(rec.ID=aID) then exit;
      if CheckFailed(not fBool.GetBoolean(Pointer(rec.SBF))) then exit;
      rec.SetFieldSBFValue(fBool,fBool.SBF(true));
      if CheckFailed(T.RecordUpdate(rec)) then exit;
      rec := T.RecordGet(aID);
      if CheckFailed(fBool.GetBoolean(Pointer(rec.SBF))) then exit;
    end;
    T.Pack;
    if CheckFailed(TRTest) then exit;
    fInt := T.Table['int'];
    for i := 0 to nInt-1 do begin
      aID := i shl 3+1;
      rec := T.RecordGet(aID);
      if CheckFailed(rec.ID=aID) then exit;
      rec.SetFieldSBFValue(fInt,fInt.SBF(n*2+aID)); // > n -> so unique value
      if CheckFailed(T.RecordUpdate(rec)) then exit;
      if CheckFailed(fInt.GetInteger(pointer(rec.SBF))=n*2+aID) then exit;
    end;
    for i := 0 to nInt-1 do begin
      if (i and 7=0) and (i<(nInt shl 3)) then
        Value := n*2+i+1 else
        Value := i;
      IDCount := 0;
      if CheckFailed(T.Search(fInt,fInt.SBF(Value),IDs,IDCount)) or
         CheckFailed(IDCount=1) or CheckFailed(IDs[0]=i+1) then
          exit;
    end;
    result := true;
  finally
    T.Free;
  end;
  Check(not TSynBigTable.FileFormatCheck(FN));
  Check(not TSynBigTableString.FileFormatCheck(FN));
  if DoRecord then
    Check(TSynBigTableRecord.FileFormatCheck(FN)) else
    Check(TSynBigTableMetaData.FileFormatCheck(FN));
end;

function TTestBigTable.TestStream(aID: integer; Data: TStream): boolean;
var i: integer;
begin
  result := false;
  if not Data.InheritsFrom(TCustomMemoryStream) then
    exit;
  if FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,aID)>=0 then
    inc(aID,$01010101); // an updated ID has had its content refreshed
  with TCustomMemoryStream(Data) do
    for i := 0 to (Size shr 2)-1 do
      if PIntegerArray(Memory)^[i]<>aID then
        exit;
  result := true;
  Data.Free;
end;

function TTestBigTable.TestString(aID: integer; const Data: RawByteString): boolean;
var i: integer;
begin
  result := false;
  if FastFindIntegerSorted(pointer(Updated),UpdatedCount-1,aID)>=0 then
    inc(aID,$01010101); // an updated ID has had its content refreshed
  for i := 0 to (length(Data) shr 2)-1 do
    if PIntegerArray(Data)^[i]<>aID then
      exit;
  result := true;
end;

procedure TTestBigTable._TSynBigTable;
var i,n,id,tot: integer;
    T: TSynBigTable;
    Data: RawByteString;
begin
  n := Random(10)+1000;
  UpdatedCount := 0;
  FN := ChangeFileExt(paramstr(0),'.syn1');
  DeleteFile(FN);
  T := TSynBigTable.Create(FN);
  try
    for i := 1 to n do
      if not CheckFailed(T.Add(CreateString(i))=i) then
      if T.CurrentInMemoryDataSize>10 shl 20 then // write on disk every 10 MB
        T.UpdateToFile;
    Check(T.Count=n);
    for i := 1 to n do
      if CheckFailed(T.Get(i,Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    T.Free;
    T := TSynBigTable.Create(FN); // verify after read
    for i := 1 to n do begin
      id := Random(n)+1; // test random access speed
      if CheckFailed(T.Get(id,Data)) or CheckFailed(TestString(id,Data)) then
        exit;
    end;
    for i := 1 to n do
      if CheckFailed(T.Get(i,Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    for i := n+1 to n+100 do // create some fInMemory[] data before Pack
      Check(T.Add(CreateString(i))=i,'Adding 100 elements');
    for i := n+100 downto n+1 do // update fInMemory[] in-place data
      Check(T.Update(i,CreateString(i))=i,'Updating 100 elements');
    inc(n,100);
    if not Delete(T,n,true,false) then
      exit;
    T.Free;
    T := TSynBigTable.Create(FN); // verify after read
    for i := 1 to n do
      if IntegerScanIndex(pointer(Deleted),length(Deleted),i)<0 then
      if CheckFailed(T.Get(i,Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    Check(Delete(T,n,false,false));
    T.Pack; // delete data from disk
    tot := 0;
    T.GetIterating(T.TestIterateEvent,ioPhysical,@tot);
    Check(tot=T.Count,'Iteration test physical order');
    tot := 0;
    T.GetIterating(T.TestIterateEvent,ioID,@tot);
    Check(tot=T.Count,'Iteration test ID order');
    for i := 0 to T.Count-1 do
      Check(T.NumericalID[i]<>0);
  finally
    T.Free;
  end;
  Check(TSynBigTable.FileFormatCheck(FN));
  Check(not TSynBigTableString.FileFormatCheck(FN));
end;

procedure TTestBigTable._TSynBigTableMetaData;
begin
  Check(DoFieldTest(false,10000));
end;

procedure TTestBigTable._TSynBigTableRecord;
begin
  Check(DoFieldTest(true,15000));
end;

procedure TTestBigTable._TSynBigTableString;
var TS: TSynBigTableString;
    i, n, id: integer;
    Data: RawByteString;
begin
  FN := ChangeFileExt(FN,'.syn2');
  DeleteFile(FN);
  TS := TSynBigTableString.Create(FN);
  try
    UpdatedCount := 0;
    n := Random(10)+500;
    for i := 1 to n do
      if CheckFailed(TS.Add(TTestBigTable.CreateString(i),Int32ToUTF8(i))=i) then
        exit else
      if TS.CurrentInMemoryDataSize>10 shl 20 then // write on disk every 10 MB
        TS.UpdateToFile;
    Check(TS.Count=n);
    for i := 1 to n do
      if CheckFailed(TS.Get(i,Data)) or CheckFailed(TestString(i,Data)) or
         CheckFailed(TS.Get(Int32ToUTF8(i),Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    TS.Free;
    TS := TSynBigTableString.Create(FN); // verify after read
    for i := 1 to n do
      if CheckFailed(TS.Get(i,Data)) or CheckFailed(TestString(i,Data)) or
         CheckFailed(TS.Get(Int32ToUTF8(i),Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    for i := 1 to n do begin
      id := Random(n)+1; // test random access
      if CheckFailed(TS.Get(id,Data)) or CheckFailed(TestString(id,Data)) or
         CheckFailed(TS.Get(Int32ToUTF8(id),Data)) or CheckFailed(TestString(id,Data)) then
        exit;
    end;
    for i := n+1 to n+100 do // create some fInMemory[] data before Pack
      Check(TS.Add(TTestBigTable.CreateString(i),Int32ToUTF8(i))=i);
    inc(n,100);
    for i := 1 to n do
      if CheckFailed(TS.Get(i,Data)) or CheckFailed(TestString(i,Data)) or
         CheckFailed(TS.Get(Int32ToUTF8(i),Data)) or CheckFailed(TestString(i,Data)) then
        exit;
    if CheckFailed(Delete(TSynBigTable(TS),n,true,true)) then
      exit;
    TS.Free;
    TS := TSynBigTableString.Create(FN); // verify after read
    Check(Delete(TSynBigTable(TS),n,false,true));
  finally
    TS.Free;
  end;
  Check(not TSynBigTable.FileFormatCheck(FN));
  Check(TSynBigTableString.FileFormatCheck(FN));
end;


{ TSynBigTableTable }

function TSynBigTableTable.AddField(const aName: RawUTF8; aType: TSynTableFieldType;
  aOptions: TSynTableFieldOptions): boolean;
begin
  if (self=nil) or (Table=nil) then
    result := false else
    if (tfoUnique in aOptions) and (Count>0) then
      raise Exception.CreateFmt(
        'Impossible to create an UNIQUE %s field with existing data',[aName]) else
      result := Table.AddField(aName,aType,aOptions)<>nil;
end;

function TSynBigTableTable.CheckConstraints(const aRecordData: RawByteString;
  RecordIndex: integer): boolean;
begin
  if (self=nil) or (aRecordData='') or (Table=nil) then
    // invalid data (direct Add/Update call with no SBF format)
    result := false else
    result := Table.Validate(pointer(aRecordData),RecordIndex)='';
end;

procedure TSynBigTableTable.Clear;
begin
  if (self<>nil) and (Table<>nil) then
    Table.FieldList.Clear;
  inherited Clear;
end;

constructor TSynBigTableTable.Create(const aFileName: TFileName;
  const aTableName: RawUTF8; GetRecordData: TSynTableGetRecordData;
  FileOpenMode: Cardinal);
begin
  fTable := TSynTable.Create(aTableName);
  fTable.GetRecordData := GetRecordData;
  inherited Create(aFileName,FileOpenMode);
  if aTableName<>'' then begin
    fTableName := aTableName;
    Table.TableName := aTableName;
  end;
end;

destructor TSynBigTableTable.Destroy;
begin
  inherited;
  fTable.Free;
end;

type
  PIterateSearchOpaque = ^TIterateSearchOpaque;
  TIterateSearchOpaque = record
    Values: PIntegerDynArray;
    Count: PInteger;
    Search: pointer;
    Field: TSynTableFieldProperties;
    Limit: Integer;
  end;

function TSynBigTableTable.IterateSearch(Sender: TObject; Opaque: pointer;
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
begin
  with PIterateSearchOpaque(Opaque)^ do
    if Field.SortCompare(Table.GetData(Data,Field),Search)=0 then begin
      AddSortedInteger(Values^,Count^,ID);
      if Count^>=Limit then begin
        result := false; // we have enough results -> stop iteration
        exit;
      end;
    end;
  result := true;
end;

function TSynBigTableTable.RecordGet(aID: integer): TSynTableData;
begin
  RecordGet(aId,result);
end;

{$ifndef LVCL}
function TSynBigTableTable.Search(Field: TSynTableFieldProperties;
  const WhereValue: variant; var ResultID: TIntegerDynArray;
  var ResultIDCount: integer; Limit: Integer;
  ForceIterate: TSynBigTableIterationOrder): boolean;
begin
  result := Search(Field,Field.SBF(WhereValue),ResultID,ResultIDCount,Limit,ForceIterate);
end;
{$endif}

function TSynBigTableTable.Search(Field: TSynTableFieldProperties;
  const WhereValue: TSBFString; var ResultID: TIntegerDynArray;
  var ResultIDCount: integer; Limit: Integer;
  ForceIterate: TSynBigTableIterationOrder): boolean;
var MatchIndex: TIntegerDynArray;
    MatchCount, i, aID: integer;
    Opaque: TIterateSearchOpaque;
begin
  if ResultID=nil then
    ResultIDCount := 0; // if caller forgot to initialize the count
  if (self=nil) or (Field=nil) or (WhereValue='') then
    result := false else
  if (tfoIndex in Field.Options) and (ForceIterate in [ioNone,ioFaster]) then begin
    MatchCount := 0;
    result := Field.OrderedIndexMatch(pointer(WhereValue),MatchIndex,MatchCount,Limit);
    if not result then
      exit;
    for i := 0 to MatchCount-1 do begin
      aID := IndexToID(MatchIndex[i]);
      if aID>0 then
        AddSortedInteger(ResultID,ResultIDCount,aID);
    end;
  end else begin
    Opaque.Count := @ResultIDCount;
    Opaque.Values := @ResultID;
    Opaque.Field := Field;
    Opaque.Search := pointer(WhereValue);
    if Limit=0 then // no LIMIT set -> retrieve all rows
      Opaque.Limit := maxInt else
      Opaque.Limit := Limit;
    SearchIterating(IterateSearch,ForceIterate,@Opaque);
    result := true;
  end;
end;

{$ifndef LVCL}
function TSynBigTableTable.VariantVoid: Variant;
begin
  if (self=nil) or (fTable=nil) then
    VarClear(result) else
    result := fTable.Data;
end;
{$endif}


{ TSynBigTableMetaData }

function TSynBigTableMetaData.Add(const aData: RawByteString;
  const aMetaData: TSBFString): integer;
var ndx: integer;
begin
  if not CheckConstraints(aMetaData,-1) then
    result := 0 else begin
    result := inherited Add(aData);
    if result>0 then begin
      ndx := fMetaDataCount;
      if ndx>=length(fMetaDataRecords) then begin
        inc(ndx,ndx+256+ndx shr 3);
        SetLength(fMetaDataRecords,ndx);
        SetLength(fMetaDataID,ndx);
        ndx := fMetaDataCount;
      end;
      inc(fMetaDataCount);
      fMetaDataRecords[ndx] := aMetaData;
      fMetaDataID[ndx] := Result;
      Table.FieldIndexModify(-1,ndx,nil,nil);
    end;
  end;
end;

procedure TSynBigTableMetaData.AddFieldUpdate;
var i, F: Integer;
    AvailableFields: TSQLFieldBits;
begin
  if (self=nil) or (Table.AddedField=nil) then
    exit; // nothing new
  if fMetaDataCount=0 then begin // no data to refresh
    Table.AddedField.Free; // do it once
    Table.AddedField:= nil;
    exit;
  end;
  fillchar(AvailableFields,sizeof(AvailableFields),0);
  for F := 0 to Table.FieldCount-1 do
    if Table.AddedField.IndexOf(Table.Field[F])<0 then
      Include(AvailableFields,F);
  Table.AddedField.Free; // do it once
  Table.AddedField := nil;
  for i := 0 to Count-1 do
    fMetaDataRecords[i] := Table.UpdateFieldRecord(pointer(fMetaDataRecords[i]),AvailableFields);
  fModified := true; 
end;

constructor TSynBigTableMetaData.Create(const aFileName: TFileName;
  const aTableName: RawUTF8; FileOpenMode: Cardinal);
begin
  inherited Create(aFileName,aTableName,GetMetaData,FileOpenMode);
end;

function TSynBigTableMetaData.CustomHeader(Action: TSynBigTableCustomHeader;
  WriteBuffer: TFileBufferWriter): integer;
var F,n: integer;
begin
  result := 0; // make compiler happy
  case Action of
    sbtBeforeWrite:
      for F := 0 to Table.FieldCount-1 do
        Table.Field[F].OrderedIndexRefresh;
    sbtWrite:
      if WriteBuffer<>nil then begin
        Table.SaveTo(WriteBuffer);
        WriteBuffer.WriteRawUTF8DynArray(TRawUTF8DynArray(fMetaDataRecords),fMetaDataCount);
        WriteBuffer.WriteVarUInt32Array(fMetaDataID,fMetaDataCount,wkSorted);
      end;
    sbtRead: begin
      Table.LoadFrom(fReadBuffer);
      fTableName := Table.TableName;
      fMetaDataCount := fReadBuffer.ReadVarRawUTF8DynArray(TRawUTF8DynArray(fMetaDataRecords));
      if (fMetaDataCount<>inherited GetCount) or
         (fReadBuffer.ReadVarUInt32Array(fMetaDataID)<>fMetaDataCount) then
        fReadBuffer.ErrorInvalidContent;
    end;
    sbtAfterRead: begin
      n := Count;
      if n>0 then
      for F := 0 to Table.FieldCount-1 do
        with Table.Field[F] do
        if tfoIndex in Options then
          if (OrderedIndexCount<>n) or (length(OrderedIndex)<n) then begin
            // this index need to be recreated -> do it now
            OrderedIndexCount := n;
            SetLength(OrderedIndex,n);
            FillIncreasing(pointer(OrderedIndex),0,n);
            OrderedIndexUpdate(-1,-1,nil,nil); // both indexes equal -1 -> just sort
            fModified := true;
          end;
    end;
  end;
end;

function TSynBigTableMetaData.Delete(aID: integer; PhysicalIndex: PInteger): boolean;
var ndx: integer;
begin
  ndx := FastFindIntegerSorted(pointer(fMetaDataID),fMetaDataCount-1,aID);
  if ndx<0 then
    Result := false else begin
    Result := inherited Delete(aID);
    if Result then begin
      DeleteInteger(fMetaDataID,fMetaDataCount,ndx);
      DeleteRawUTF8(TRawUTF8DynArray(fMetaDataRecords),fMetaDataCount,ndx);
      Table.FieldIndexModify(ndx,-1,nil,nil);
    end;
  end;
end;

function TSynBigTableMetaData.GetCount: integer;
begin
  result := fMetaDataCount;
end;

function TSynBigTableMetaData.GetID(Index: integer): integer;
begin
  if cardinal(Index)<Cardinal(fMetaDataCount) then
    result := fMetaDataID[Index] else
    result := 0;
end;

function TSynBigTableMetaData.GetMetaData(aPhysicalIndex: integer;
  var aTempData: RawByteString): pointer;
begin
  if cardinal(aPhysicalIndex)<Cardinal(fMetaDataCount) then
    result := Pointer(fMetaDataRecords[aPhysicalIndex]) else
    result := nil;
end;

function TSynBigTableMetaData.GetMetaDataFromID(aID: integer): pointer;
var ndx: integer;
begin
  ndx := FastFindIntegerSorted(pointer(fMetaDataID),fMetaDataCount-1,aID);
  if ndx<0 then
    result := nil else
    result := pointer(fMetaDataRecords[ndx]);
end;

function TSynBigTableMetaData.IndexToID(aIndex: Integer;
  handleAliases: boolean): integer;
begin
  if cardinal(aIndex)<cardinal(fMetaDataCount) then
    result := fMetaDataID[aIndex] else
    result := 0; 
end;

class function TSynBigTableMetaData.InternalMagic: integer;
begin
  result := integer($A5A50002); // genuine ID which differs from others
end;

function TSynBigTableMetaData.RecordAdd(const aData: RawByteString;
  const aMetaDataRecord: TSynTableData): integer;
begin
  result := Add(aData,aMetaDataRecord.SBF);
end;

procedure TSynBigTableMetaData.RecordGet(aID: integer;
  var result: TSynTableData);
var RecordSBF: RawByteString;
    ndx: integer;
begin
  ndx := FastFindIntegerSorted(pointer(fMetaDataID),fMetaDataCount-1,aID);
  if ndx>=0 then
    RecordSBF := fMetaDataRecords[ndx];
  result.Init(Table,aID,pointer(RecordSBF),length(RecordSBF));
end;

function TSynBigTableMetaData.RecordUpdate(const aMetaDataRecord: TSynTableData): boolean;
begin
  result := Update(aMetaDataRecord.ID,aMetaDataRecord.SBF);
end;

procedure TSynBigTableMetaData.SearchIterating(aCallBack: TSynBigTableIterateEvent;
  Order: TSynBigTableIterationOrder; Opaque: pointer);
var i: integer;
begin // only handle internal order by now (i.e. ioID)
  for i := 0 to fMetaDataCount-1 do
    if not aCallBack(self,Opaque,fMetaDataID[i],i,
       pointer(fMetaDataRecords[i]),length(fMetaDataRecords[i])) then
      exit;
end;

function TSynBigTableMetaData.Update(aID: integer; const aMetaData: TSBFString): boolean;
var ndx: integer;
    aOldData: TSBFString;
begin
  ndx := FastFindIntegerSorted(pointer(fMetaDataID),fMetaDataCount-1,aID);
  if (ndx>=0) and CheckConstraints(aMetaData,ndx) then begin
    aOldData := fMetaDataRecords[ndx];
    fMetaDataRecords[ndx] := aMetaData;
    Table.FieldIndexModify(ndx,ndx,pointer(aOldData),pointer(aMetaData));
    result := true;
  end else
    result := false;
end;

{$ifndef LVCL}
function TSynBigTableMetaData.VariantAdd(const aData: RawByteString;
  const aMetaDataRecord: Variant): integer;
begin
  result := Add(aData,TSynTableVariantType.ToSBF(aMetaDataRecord));
end;

function TSynBigTableMetaData.VariantGet(aID: integer): Variant;
var ndx: Integer;
    RecordSBF: RawByteString;
begin
  ndx := FastFindIntegerSorted(pointer(fMetaDataID),fMetaDataCount-1,aID);
  if ndx>=0 then
    RecordSBF := fMetaDataRecords[ndx];
  if RecordSBF='' then
    VarClear(result) else
    result := fTable.Data(aID,pointer(RecordSBF),length(RecordSBF));
end;

function TSynBigTableMetaData.VariantUpdate(const aMetaDataRecord: Variant): boolean;
begin
  with TSynTableVariantType do
   result := Update(ToID(aMetaDataRecord),ToSBF(aMetaDataRecord));
end;
{$endif}

end.

