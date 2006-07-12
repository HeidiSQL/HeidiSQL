//-----------------------------------------------------------------
//   SchemaDef.h - defines needed to build a Theme Manager schema
//                file
//-----------------------------------------------------------------
#ifndef SCHEMA_STRINGS           // FIRST PASS of this hdr file
//-----------------------------------------------------------------
#ifndef SCHEMADEF_H
#define SCHEMADEF_H
//-----------------------------------------------------------------
#define SCHEMADEF_VERSION   1    // defines the exported func(s) implemented
//-----------------------------------------------------------------
struct TMPROPINFO
{
    LPCWSTR pszName;
    SHORT sEnumVal;
    BYTE bPrimVal;
};
//-----------------------------------------------------------------
struct TMSCHEMAINFO
{
    DWORD dwSize;               // size of this struct
    int iSchemaDefVersion;      // version number from this file
    int iThemeMgrVersion;       // version number from "thschema.h"
    int iPropCount;             // # of entries in prop table
    const struct TMPROPINFO *pPropTable;       // ptr to prop table 
};
//---------------------------------------------------------------------------
#define BEGIN_TM_SCHEMA(name)               
#define BEGIN_TM_PROPS()                    enum PropValues { DummyProp = 49,
#define BEGIN_TM_ENUM(name)                 enum name {
#define BEGIN_TM_CLASS_PARTS(name)          enum name##PARTS { name##PartFiller0,
#define BEGIN_TM_PART_STATES(name)          enum name##STATES { name##StateFiller0,

#define TM_PROP(val, prefix, name, primval) prefix##_##name = val, 
#define TM_ENUM(val, prefix, name)          prefix##_##name = val,
#define TM_PART(val, prefix, name)          prefix##_##name = val, 
#define TM_STATE(val, prefix, name)         prefix##_##name = val, 

#define END_TM_CLASS_PARTS()                };
#define END_TM_PART_STATES()                };
#define END_TM_PROPS()                      };
#define END_TM_ENUM()                       };
#define END_TM_SCHEMA(name)
//---------------------------------------------------------------------------
#endif      // SCHEMADEF_H
//---------------------------------------------------------------------------
#else                   // SECOND PASS of this hdr file
//---------------------------------------------------------------------------
#undef BEGIN_TM_SCHEMA
#undef BEGIN_TM_PROPS
#undef BEGIN_TM_ENUM
#undef BEGIN_TM_CLASS_PARTS
#undef BEGIN_TM_PART_STATES
#undef TM_PROP
#undef TM_PART
#undef TM_STATE
#undef TM_ENUM
#undef END_TM_CLASS_PARTS
#undef END_TM_PART_STATES
#undef END_TM_PROPS
#undef END_TM_ENUM
#undef END_TM_SCHEMA
//---------------------------------------------------------------------------
#define BEGIN_TM_SCHEMA(name)              static const TMPROPINFO name[] = {
#define BEGIN_TM_PROPS()   
#define BEGIN_TM_ENUM(name)                 {L#name, TMT_ENUMDEF, TMT_ENUMDEF},
#define BEGIN_TM_CLASS_PARTS(name)          {L#name L"PARTS", TMT_ENUMDEF, TMT_ENUMDEF},
#define BEGIN_TM_PART_STATES(name)          {L#name L"STATES", TMT_ENUMDEF, TMT_ENUMDEF},

#define TM_PROP(val, prefix, name, primval) {L#name, prefix##_##name, TMT_##primval},
#define TM_PART(val, prefix, name)          {L#name, prefix##_##name, TMT_ENUMVAL},
#define TM_STATE(val, prefix, name)         {L#name, prefix##_##name, TMT_ENUMVAL},
#define TM_ENUM(val, prefix, name)          {L#name, prefix##_##name, TMT_ENUMVAL},

#define END_TM_CLASS_PARTS() 
#define END_TM_PART_STATES() 
#define END_TM_PROPS() 
#define END_TM_ENUM()
#define END_TM_SCHEMA(name)                };  \
    static const TMSCHEMAINFO *GetSchemaInfo() \
    {  \
        static TMSCHEMAINFO si = {sizeof(si)};  \
        si.iSchemaDefVersion = SCHEMADEF_VERSION; \
        si.iThemeMgrVersion = THEMEMGR_VERSION; \
        si.iPropCount = sizeof(name)/sizeof(name[0]); \
        si.pPropTable = name; \
        \
        return &si; \
    }

//-----------------------------------------------------------------
#endif
//-----------------------------------------------------------------
