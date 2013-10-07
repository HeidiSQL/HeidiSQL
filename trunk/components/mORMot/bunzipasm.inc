
{
    bzlib2 decompression functions
    ==============================
      - more than 2 times faster than standard C version
      - no external .obj necessary
      - 100% API compatible
      - does CRC check
      - fully tested under Delphi, work with (Cross)Kylix

  asm translation extracted from isbunzip.dll and optimized for Delphi 7

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info

  All Rights Reserved.

}

procedure BzExcept;
begin
  raise Exception.Create('BZ decompress error');
end;

const
  _BZ2_crc32Table: array[byte] of cardinal = ( // not the same as ZLib !
   $00000000, $04c11db7, $09823b6e, $0d4326d9,
   $130476dc, $17c56b6b, $1a864db2, $1e475005,
   $2608edb8, $22c9f00f, $2f8ad6d6, $2b4bcb61,
   $350c9b64, $31cd86d3, $3c8ea00a, $384fbdbd,
   $4c11db70, $48d0c6c7, $4593e01e, $4152fda9,
   $5f15adac, $5bd4b01b, $569796c2, $52568b75,
   $6a1936c8, $6ed82b7f, $639b0da6, $675a1011,
   $791d4014, $7ddc5da3, $709f7b7a, $745e66cd,
   $9823b6e0, $9ce2ab57, $91a18d8e, $95609039,
   $8b27c03c, $8fe6dd8b, $82a5fb52, $8664e6e5,
   $be2b5b58, $baea46ef, $b7a96036, $b3687d81,
   $ad2f2d84, $a9ee3033, $a4ad16ea, $a06c0b5d,
   $d4326d90, $d0f37027, $ddb056fe, $d9714b49,
   $c7361b4c, $c3f706fb, $ceb42022, $ca753d95,
   $f23a8028, $f6fb9d9f, $fbb8bb46, $ff79a6f1,
   $e13ef6f4, $e5ffeb43, $e8bccd9a, $ec7dd02d,
   $34867077, $30476dc0, $3d044b19, $39c556ae,
   $278206ab, $23431b1c, $2e003dc5, $2ac12072,
   $128e9dcf, $164f8078, $1b0ca6a1, $1fcdbb16,
   $018aeb13, $054bf6a4, $0808d07d, $0cc9cdca,
   $7897ab07, $7c56b6b0, $71159069, $75d48dde,
   $6b93dddb, $6f52c06c, $6211e6b5, $66d0fb02,
   $5e9f46bf, $5a5e5b08, $571d7dd1, $53dc6066,
   $4d9b3063, $495a2dd4, $44190b0d, $40d816ba,
   $aca5c697, $a864db20, $a527fdf9, $a1e6e04e,
   $bfa1b04b, $bb60adfc, $b6238b25, $b2e29692,
   $8aad2b2f, $8e6c3698, $832f1041, $87ee0df6,
   $99a95df3, $9d684044, $902b669d, $94ea7b2a,
   $e0b41de7, $e4750050, $e9362689, $edf73b3e,
   $f3b06b3b, $f771768c, $fa325055, $fef34de2,
   $c6bcf05f, $c27dede8, $cf3ecb31, $cbffd686,
   $d5b88683, $d1799b34, $dc3abded, $d8fba05a,
   $690ce0ee, $6dcdfd59, $608edb80, $644fc637,
   $7a089632, $7ec98b85, $738aad5c, $774bb0eb,
   $4f040d56, $4bc510e1, $46863638, $42472b8f,
   $5c007b8a, $58c1663d, $558240e4, $51435d53,
   $251d3b9e, $21dc2629, $2c9f00f0, $285e1d47,
   $36194d42, $32d850f5, $3f9b762c, $3b5a6b9b,
   $0315d626, $07d4cb91, $0a97ed48, $0e56f0ff,
   $1011a0fa, $14d0bd4d, $19939b94, $1d528623,
   $f12f560e, $f5ee4bb9, $f8ad6d60, $fc6c70d7,
   $e22b20d2, $e6ea3d65, $eba91bbc, $ef68060b,
   $d727bbb6, $d3e6a601, $dea580d8, $da649d6f,
   $c423cd6a, $c0e2d0dd, $cda1f604, $c960ebb3,
   $bd3e8d7e, $b9ff90c9, $b4bcb610, $b07daba7,
   $ae3afba2, $aafbe615, $a7b8c0cc, $a379dd7b,
   $9b3660c6, $9ff77d71, $92b45ba8, $9675461f,
   $8832161a, $8cf30bad, $81b02d74, $857130c3,
   $5d8a9099, $594b8d2e, $5408abf7, $50c9b640,
   $4e8ee645, $4a4ffbf2, $470cdd2b, $43cdc09c,
   $7b827d21, $7f436096, $7200464f, $76c15bf8,
   $68860bfd, $6c47164a, $61043093, $65c52d24,
   $119b4be9, $155a565e, $18197087, $1cd86d30,
   $029f3d35, $065e2082, $0b1d065b, $0fdc1bec,
   $3793a651, $3352bbe6, $3e119d3f, $3ad08088,
   $2497d08d, $2056cd3a, $2d15ebe3, $29d4f654,
   $c5a92679, $c1683bce, $cc2b1d17, $c8ea00a0,
   $d6ad50a5, $d26c4d12, $df2f6bcb, $dbee767c,
   $e3a1cbc1, $e760d676, $ea23f0af, $eee2ed18,
   $f0a5bd1d, $f464a0aa, $f9278673, $fde69bc4,
   $89b8fd09, $8d79e0be, $803ac667, $84fbdbd0,
   $9abc8bd5, $9e7d9662, $933eb0bb, $97ffad0c,
   $afb010b1, $ab710d06, $a6322bdf, $a2f33668,
   $bcb4666d, $b8757bda, $b5365d03, $b1f740b4 );

  _BZ2_rNums: array[0..511] of cardinal = (
   619, 720, 127, 481, 931, 816, 813, 233, 566, 247, 
   985, 724, 205, 454, 863, 491, 741, 242, 949, 214, 
   733, 859, 335, 708, 621, 574, 73, 654, 730, 472, 
   419, 436, 278, 496, 867, 210, 399, 680, 480, 51,
   878, 465, 811, 169, 869, 675, 611, 697, 867, 561, 
   862, 687, 507, 283, 482, 129, 807, 591, 733, 623, 
   150, 238, 59, 379, 684, 877, 625, 169, 643, 105, 
   170, 607, 520, 932, 727, 476, 693, 425, 174, 647, 
   73, 122, 335, 530, 442, 853, 695, 249, 445, 515, 
   909, 545, 703, 919, 874, 474, 882, 500, 594, 612, 
   641, 801, 220, 162, 819, 984, 589, 513, 495, 799, 
   161, 604, 958, 533, 221, 400, 386, 867, 600, 782, 
   382, 596, 414, 171, 516, 375, 682, 485, 911, 276, 
   98, 553, 163, 354, 666, 933, 424, 341, 533, 870, 
   227, 730, 475, 186, 263, 647, 537, 686, 600, 224, 
   469, 68, 770, 919, 190, 373, 294, 822, 808, 206, 
   184, 943, 795, 384, 383, 461, 404, 758, 839, 887, 
   715, 67, 618, 276, 204, 918, 873, 777, 604, 560, 
   951, 160, 578, 722, 79, 804, 96, 409, 713, 940, 
   652, 934, 970, 447, 318, 353, 859, 672, 112, 785, 
   645, 863, 803, 350, 139, 93, 354, 99, 820, 908, 
   609, 772, 154, 274, 580, 184, 79, 626, 630, 742, 
   653, 282, 762, 623, 680, 81, 927, 626, 789, 125, 
   411, 521, 938, 300, 821, 78, 343, 175, 128, 250,
   170, 774, 972, 275, 999, 639, 495, 78, 352, 126, 
   857, 956, 358, 619, 580, 124, 737, 594, 701, 612,
   669, 112, 134, 694, 363, 992, 809, 743, 168, 974, 
   944, 375, 748, 52, 600, 747, 642, 182, 862, 81, 
   344, 805, 988, 739, 511, 655, 814, 334, 249, 515, 
   897, 955, 664, 981, 649, 113, 974, 459, 893, 228, 
   433, 837, 553, 268, 926, 240, 102, 654, 459, 51, 
   686, 754, 806, 760, 493, 403, 415, 394, 687, 700,
   946, 670, 656, 610, 738, 392, 760, 799, 887, 653, 
   978, 321, 576, 617, 626, 502, 894, 679, 243, 440,
   680, 879, 194, 572, 640, 724, 926, 56, 204, 700, 
   707, 151, 457, 449, 797, 195, 791, 558, 945, 679, 
   297, 59, 87, 824, 713, 663, 412, 693, 342, 606, 
   134, 108, 571, 364, 631, 212, 174, 643, 304, 329, 
   343, 97, 430, 751, 497, 314, 983, 374, 822, 928, 
   140, 206, 73, 263, 980, 736, 876, 478, 430, 305, 
   170, 514, 364, 692, 829, 82, 855, 953, 676, 246, 
   369, 970, 294, 750, 807, 827, 150, 790, 288, 923, 
   804, 378, 215, 828, 592, 281, 565, 555, 710, 82, 
   896, 831, 547, 261, 524, 462, 293, 465, 502, 56, 
   661, 821, 976, 991, 658, 869, 905, 758, 745, 193, 
   768, 550, 608, 933, 378, 286, 215, 979, 792, 961, 
   61, 688, 793, 644, 986, 403, 106, 366, 905, 644, 
   372, 567, 466, 434, 645, 210, 389, 550, 919, 135, 
   780, 773, 635, 389, 707, 100, 626, 958, 165, 504,
   920, 176, 193, 713, 857, 265, 203, 50, 668, 108, 
   645, 990, 626, 197, 510, 357, 358, 850, 858, 364, 
   936, 638 ); // from original randtable.c


function BZ2_bzDecompressInit(var strm: TBZStreamRec;
    verbosity, small: Integer): Integer; cdecl;
asm pop ebp  // auto-generated push ebp; mov ebp,esp
        push    ebx
        push    ebp
        push    esi
        push    edi
@002:   mov     esi,[esp + 14H]
        xor     edi, edi
        cmp     esi, edi
        je      @005
        mov     ebx,[esp + 1CH]
        cmp     ebx, edi
        jz      @003
        cmp     ebx, 1
        jnz     @005
@003:   mov     ebp,[esp + 18H]
        cmp     ebp, edi
        jl      @005
        cmp     ebp, 4
        jg      @005
        mov     eax,[esi + 24H]
        cmp     eax, edi
        jz      @005
        cmp    [esi + 28H], edi
        jz      @005
        mov     ecx,[esi + 2CH]
        push    1
        push    64116
        push    ecx
        call    eax
        cmp     eax, edi
        jnz     @004
        pop     edi
        pop     esi
        pop     ebp
        mov     eax, -3
        pop     ebx
        ret                                         

@004:   mov    [eax], esi                  
        mov    [esi + 20H], eax            
        mov    dword ptr [eax + 04H], 10
        mov    [eax + 20H], edi            
        mov    [eax + 1CH], edi            
        mov    [eax + 00000C64H], edi      
        mov    [esi + 08H], edi            
        mov    [esi + 0CH], edi
        mov    [esi + 18H], edi
        mov    [esi + 1CH], edi
        mov    [eax + 00000C54H], edi
        mov    [eax + 00000C50H], edi
        mov    [eax + 00000C4CH], edi
        mov    [eax + 2CH], edi
        pop     edi
        mov    [eax + 30H], ebp
        pop     esi
        mov    [eax + 28H], bl
        pop     ebp
        xor     eax, eax
        pop     ebx
        ret

@005:   pop     edi
        pop     esi
        pop     ebp
        mov     eax, -2
        pop     ebx
        ret
end;

function BZ2_bzDecompress(var strm: TBZStreamRec): Integer; cdecl;
asm pop ebp  // auto-generated push ebp; mov ebp,esp
        mov     eax,[esp + 04H]
        push    esi
        test    eax, eax
        jnz     @010
        mov     eax, -2
        pop     esi
        ret

@010:   mov     esi,[eax + 20H]
        test    esi, esi
        jnz     @011
        mov     eax, -2
        pop     esi
        ret     

@011:   cmp    [esi], eax
        jz      @012
        mov     eax, -2
        pop     esi
        ret     

@012:   mov     eax,[esi + 04H]
        cmp     eax, 1
        je      @016
        cmp     eax, 2
        jnz     @015
        mov     al,[esi + 28H]
        push    esi
        test    al, al
        jz      @013
        call    @048
        jmp     @014

@013:   call    @021
@014:   test    al, al                                
        jnz     @017                                 
        mov     eax,[esi + 0000FA40H]      
        mov     ecx,[esi + 00000440H]      
        inc     eax                                   
        cmp     ecx, eax                              
        jnz     @018                                 
        mov     eax,[esi + 0CH]            
        test    eax, eax                              
        jnz     @018                                 
        mov     ecx,[esi + 00000C60H]      
        mov     eax,[esi + 00000C58H]      
        not     ecx                                   
        cmp     ecx, eax                              
        mov    [esi + 00000C60H], ecx      
        jnz     @017                                 
        mov     eax,[esi + 00000C64H]
        mov     dword ptr [esi + 04H], 14             
        mov     edx, eax                              
        add     eax, eax                              
        shr     edx, 31                               
        or      edx, eax
        xor     edx, ecx
        mov    [esi + 00000C64H], edx
@015:   cmp     dword ptr [esi + 04H], 10
        jl      @012
        push    esi
        call    @070
        cmp     eax, 4                                
        jz      @019                                 
        cmp     dword ptr [esi + 04H], 2
        jnz     @020                                 
        jmp     @012                                 

@016:   or      eax, 0FFFFFFFFH
        pop     esi                                   
        ret                                          

@017:   mov     eax, -4
        pop     esi                                   
        ret                                          

@018:   xor     eax, eax
        pop     esi                                   
        ret                                          

@019:   mov     eax,[esi + 00000C64H]      
        mov     ecx,[esi + 00000C5CH]      
        sub     eax, ecx                              
        neg     eax
        sbb     eax, eax
        and     al, 0F8H
        add     eax, 4
@020:   pop     esi
        ret

@021:   mov     eax,[esp + 04H]
        sub     esp, 28
        mov     cl,[eax + 10H]
        push    ebx
        push    ebp
        push    esi
        test    cl, cl
        push    edi
        je      @032
@022:   mov     ecx,[eax]
        mov     edx,[ecx + 14H]
        test    edx, edx
        je      @040
        mov     edx,[eax + 0CH]
        test    edx, edx
        jz      @023
        mov     ecx,[ecx + 10H]
        mov     dl,[eax + 08H]
        xor     ebx, ebx
        mov    [ecx], dl                    
        mov     ecx,[eax + 00000C60H]      
        mov     bl,[eax + 08H]              
        mov     edx, ecx                              
        mov     ebp,[eax + 0CH]            
        shr     edx, 24
        xor     edx, ebx                              
        shl     ecx, 8
        mov     edx,dword ptr[_BZ2_crc32Table + edx*4]
        xor     edx, ecx                              
        mov     ecx,[eax]                  
        dec     ebp                                   
        mov    [eax + 00000C60H], edx      
        mov    [eax + 0CH], ebp            
        mov     ebx,[ecx + 10H]            
        inc     ebx                                   
        mov    [ecx + 10H], ebx            
        mov     ecx,[eax]
        mov     edi,[ecx + 14H]            
        dec     edi                                   
        mov    [ecx + 14H], edi
        mov     ecx,[eax]
        mov     esi,[ecx + 18H]            
        inc     esi                                   
        mov    [ecx + 18H], esi            
        mov     ecx,[eax]                  
        mov     edx,[ecx + 18H]            
        test    edx, edx                              
        jnz     @022                                 
        inc    [ecx + 1CH]                 
        jmp     @022                                 

@023:   mov     ebx,[eax + 0000FA40H]      
        mov     esi,[eax + 00000440H]
        inc     ebx
        cmp     esi, ebx                              
        je      @040                                 
        jg      @036
        mov     cl,[eax + 3CH]
        mov     ebp,[eax + 00000C4CH]      
        mov     edx,[eax + 38H]            
        mov     dword ptr [eax + 0CH], 1              
        mov    [eax + 08H], cl              
        mov     ecx,[ebp + edx*4]          
        mov    [eax + 38H], ecx            
        mov     dl, cl                                
        shr     ecx, 8                                
        mov    [eax + 38H], ecx            
        mov     ecx,[eax + 14H]            
        test    ecx, ecx                              
        jnz     @024                                 
        mov     ecx,[eax + 18H]
        mov     edi,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512                              
        mov    [eax + 14H], edi            
        mov    [eax + 18H], ecx            
        jnz     @024
        mov     dword ptr [eax + 18H], 0
@024:   mov     edi,[eax + 14H]            
        dec     edi                                   
        cmp     edi, 1
        mov    [eax + 14H], edi            
        sete    cl                                    
        xor     dl, cl                                
        inc     esi                                   
        cmp     esi, ebx
        mov    [esp + 30H], dl              
        mov    [eax + 00000440H], esi      
        je      @022                                 
        mov     ecx,[esp + 30H]            
        mov     edx,[eax + 3CH]            
        and     ecx, 000000FFH                        
        cmp     ecx, edx
        jz      @025                                 
        mov    [eax + 3CH], ecx
        jmp     @022                                 

@025:   mov     edx,[eax + 38H]            
        mov     dword ptr [eax + 0CH], 2
        mov     ecx,[ebp + edx*4]          
        mov    [eax + 38H], ecx            
        mov     dl, cl                                
        shr     ecx, 8                                
        test    edi, edi                              
        mov    [eax + 38H], ecx            
        jnz     @026
        mov     ecx,[eax + 18H]            
        mov     edi,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512                              
        mov    [eax + 14H], edi            
        mov    [eax + 18H], ecx
        jnz     @026                                 
        mov     dword ptr [eax + 18H], 0              
@026:   mov     edi,[eax + 14H]
        dec     edi
        cmp     edi, 1                                
        mov    [eax + 14H], edi            
        sete    cl                                    
        xor     dl, cl                                
        inc     esi                                   
        cmp     esi, ebx                              
        mov    [esp + 30H], dl              
        mov    [eax + 00000440H], esi
        je      @022                                 
        mov     ecx,[esp + 30H]            
        mov     edx,[eax + 3CH]            
        and     ecx, 000000FFH                        
        cmp     ecx, edx                              
        jz      @027                                 
        mov    [eax + 3CH], ecx
        jmp     @022                                 

@027:   mov     edx,[eax + 38H]
        mov     dword ptr [eax + 0CH], 3
        mov     ecx,[ebp + edx*4]          
        mov    [eax + 38H], ecx            
        mov     dl, cl                                
        shr     ecx, 8                                
        test    edi, edi                              
        mov    [eax + 38H], ecx            
        jnz     @028                                 
        mov     ecx,[eax + 18H]            
        mov     edi,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512
        mov    [eax + 14H], edi            
        mov    [eax + 18H], ecx            
        jnz     @028                                 
        mov     dword ptr [eax + 18H], 0              
@028:   mov     edi,[eax + 14H]            
        dec     edi                                   
        cmp     edi, 1
        mov    [eax + 14H], edi            
        sete    cl                                    
        xor     dl, cl                                
        inc     esi
        cmp     esi, ebx                              
        mov    [esp + 30H], dl              
        mov    [eax + 00000440H], esi      
        je      @022                                 
        mov     ecx,[esp + 30H]            
        mov     edx,[eax + 3CH]            
        and     ecx, 000000FFH                        
        cmp     ecx, edx                              
        jz      @029                                 
        mov    [eax + 3CH], ecx            
        jmp     @022                                 

@029:   mov     edx,[eax + 38H]
        mov     ecx,[ebp + edx*4]          
        mov     dl, cl                                
        mov    [eax + 38H], ecx
        mov    [esp + 30H], dl
        xor     edx, edx
        shr     ecx, 8                                
        cmp     edi, edx
        mov    [eax + 38H], ecx            
        jnz     @030                                 
        mov     ecx,[eax + 18H]            
        mov     edi,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512                              
        mov    [eax + 14H], edi            
        mov    [eax + 18H], ecx            
        jnz     @030                                 
        mov    [eax + 18H], edx
@030:   mov     ecx,[eax + 14H]            
        mov     edi,[esp + 30H]            
        dec     ecx                                   
        inc     esi                                   
        cmp     ecx, 1                                
        mov    [eax + 14H], ecx
        sete    bl                                    
        and     ebx, 000000FFH                        
        and     edi, 000000FFH                        
        xor     ebx, edi                              
        mov     edi,[eax + 38H]            
        add     ebx, 4
        mov    [eax + 00000440H], esi      
        mov    [eax + 0CH], ebx            
        mov     edi,[ebp + edi*4]
        xor     ebx, ebx                              
        mov    [eax + 38H], edi            
        mov     bl,[eax + 38H]              
        mov    [eax + 3CH], ebx            
        mov     ebx, edi                              
        shr     ebx, 8                                
        cmp     ecx, edx                              
        mov    [eax + 38H], ebx            
        jnz     @031
        mov     ecx,[eax + 18H]            
        mov     edi,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512
        mov    [eax + 14H], edi            
        mov    [eax + 18H], ecx            
        jnz     @031                                 
        mov    [eax + 18H], edx            
@031:   mov     ebx,[eax + 14H]            
        xor     edx, edx                              
        dec     ebx                                   
        mov     ecx, ebx                              
        mov    [eax + 14H], ebx            
        cmp     ecx, 1                                
        mov     ecx,[eax + 3CH]            
        sete    dl                                    
        xor     ecx, edx
        inc     esi                                   
        mov    [eax + 3CH], ecx
        mov    [eax + 00000440H], esi
        jmp     @022

@032:   mov     ebx,[eax]                  
        mov     cl,[eax + 08H]              
        mov     ebp,[eax + 00000440H]      
        mov     edi,[eax + 00000C60H]      
        mov     ebx,[ebx + 10H]            
        mov     esi,[eax + 0CH]            
        mov    [esp + 14H], ebx
        mov     ebx,[eax]                  
        mov     edx,[eax + 3CH]            
        mov    [esp + 18H], cl              
        mov     ebx,[ebx + 14H]            
        mov     ecx,[eax + 00000C4CH]      
        mov    [esp + 10H], ebx            
        mov    [esp + 28H], ebx            
        mov     ebx,[eax + 0000FA40H]      
        mov    [esp + 20H], ecx            
        mov     ecx,[eax + 38H]            
        inc     ebx
        mov    [esp + 1CH], ebp
        mov    [esp + 24H], ebx            
@033:   test    esi, esi                              
        jle     @035                                 
@034:   mov     ebx,[esp + 10H]            
        test    ebx, ebx                              
        jz      @038                                 
        cmp     esi, 1                                
        je      @043
        mov     ebp,[esp + 14H]            
        mov     bl,[esp + 18H]              
        mov    [ebp], bl                    
        mov     ebp,[esp + 18H]            
        mov     ebx, edi                              
        and     ebp, 000000FFH                        
        shr     ebx, 24                               
        xor     ebx, ebp                              
        mov     ebp,[esp + 14H]
        shl     edi, 8                                
        mov     ebx,dword ptr [_BZ2_crc32Table + ebx*4]
        xor     edi, ebx                              
        mov     ebx,[esp + 10H]
        dec     esi
        inc     ebp                                   
        dec     ebx                                   
        mov    [esp + 14H], ebp            
        mov     ebp,[esp + 1CH]            
        mov    [esp + 10H], ebx            
        jmp     @034                                 

@035:   mov     esi,[esp + 24H]            
        cmp     ebp, esi
        jle     @037                                 
@036:   pop     edi                                   
        pop     esi
        pop     ebp                                   
        mov     al, 1                                 
        pop     ebx
        add     esp, 28
        ret     4

@037:   jnz     @041
        xor     esi, esi                              
@038:   mov     ebx,[eax]                  
        mov     ebx,[ebx + 18H]            
        mov    [esp + 30H], ebx            
        mov     ebp, ebx                              
        mov     ebx,[esp + 10H]            
        sub     ebp, ebx                              
        mov     ebx,[esp + 28H]            
        add     ebp, ebx                              
        mov     ebx,[eax]                  
        mov    [ebx + 18H], ebp            
        mov     ebx,[eax]
        mov     ebp,[esp + 30H]            
        cmp    [ebx + 18H], ebp            
        jnc     @039                                 
        inc    [ebx + 1CH]
@039:   mov     bl,[esp + 18H]              
        mov    [eax + 3CH], edx            
        mov     edx,[esp + 20H]            
        mov    [eax + 0CH], esi            
        mov     esi,[esp + 1CH]            
        mov    [eax + 38H], ecx            
        mov     ecx,[eax]                  
        mov    [eax + 00000C4CH], edx      
        mov     edx,[esp + 14H]
        mov    [eax + 00000C60H], edi      
        mov    [eax + 08H], bl              
        mov    [eax + 00000440H], esi      
        mov    [ecx + 10H], edx            
        mov     eax,[eax]                  
        mov     ecx,[esp + 10H]            
        mov    [eax + 14H], ecx            
@040:   pop     edi                                   
        pop     esi
        pop     ebp                                   
        xor     al, al                                
        pop     ebx                                   
        add     esp, 28
        ret     4

@041:   mov     esi,[esp + 20H]            
        mov    [esp + 18H], dl              
        mov     ecx,[esi + ecx*4]
        mov    [esp + 30H], cl              
        mov     esi,[esp + 30H]            
        shr     ecx, 8                                
        and     esi, 000000FFH                        
        inc     ebp                                   
        cmp     esi, edx                              
        mov    [esp + 1CH], ebp            
        jz      @042
        mov     edx, esi                              
        jmp     @043

@042:   cmp     ebp,[esp + 24H]
        jnz     @045
@043:   mov     esi,[esp + 10H]            
        test    esi, esi                              
        jnz     @044                                 
        mov     esi, 1                                
        jmp     @038                                 

@044:   mov     esi,[esp + 14H]            
        mov     bl,[esp + 18H]              
        mov    [esi], bl
        mov     esi,[esp + 18H]            
        mov     ebx, edi                              
        and     esi, 000000FFH                        
        shr     ebx, 24                               
        xor     ebx, esi                              
        shl     edi, 8                                
        mov     esi,dword ptr [_BZ2_crc32Table + ebx*4]
        mov     ebx,[esp + 14H]            
        xor     edi, esi
        mov     esi,[esp + 10H]            
        inc     ebx                                   
        dec     esi                                   
        mov    [esp + 14H], ebx            
        mov    [esp + 10H], esi            
        jmp     @035                                 

@045:   mov     ebx,[esp + 20H]            
        mov     esi, 2
        mov     ecx,[ebx + ecx*4]          
        mov     ebx,[esp + 24H]            
        mov    [esp + 30H], cl              
        shr     ecx, 8                                
        inc     ebp                                   
        cmp     ebp, ebx                              
        mov    [esp + 1CH], ebp            
        je      @033                                 
        mov     ebx,[esp + 30H]
        and     ebx, 000000FFH                        
        cmp     ebx, edx                              
        jz      @046                                 
        mov     edx, ebx
        jmp     @033                                 

@046:   mov     ebx,[esp + 20H]            
        mov     esi, 3                                
        mov     ecx,[ebx + ecx*4]          
        mov     ebx,[esp + 24H]            
        mov    [esp + 30H], cl              
        shr     ecx, 8                                
        inc     ebp                                   
        cmp     ebp, ebx
        mov    [esp + 1CH], ebp            
        je      @033                                 
        mov     ebx,[esp + 30H]
        and     ebx, 000000FFH                        
        cmp     ebx, edx                              
        jz      @047
        mov     edx, ebx
        jmp     @033

@047:   mov     edx,[esp + 20H]
        mov     ecx,[edx + ecx*4]
        mov     esi, ecx
        shr     ecx, 8
        and     esi, 000000FFH
        mov     ecx,[edx + ecx*4]
        add     esi, 4
        mov     edx, ecx
        and     edx, 000000FFH
        shr     ecx, 8
        add     ebp, 2
        mov    [esp + 1CH], ebp
        jmp     @033

@048:   push    ebx                                   
        push    ebp                                   
        push    esi
        mov     esi,[esp + 10H]            
        push    edi                                   
        mov     ebx, 1                                
        mov     al,[esi + 10H]              
        test    al, al                                
        je      @060                                 
@049:   mov     eax,[esi]                  
        mov     ecx,[eax + 14H]            
        test    ecx, ecx
        je      @062                                 
        mov     ecx,[esi + 0CH]            
        test    ecx, ecx                              
        jz      @050                                 
        mov     eax,[eax + 10H]            
        mov     cl,[esi + 08H]              
        mov    [eax], cl
        mov     eax,[esi + 00000C60H]      
        mov     edx, eax
        xor     ecx, ecx                              
        mov     cl,[esi + 08H]              
        shr     edx, 24                               
        xor     edx, ecx
        shl     eax, 8                                
        mov     edx,dword ptr [_BZ2_crc32Table + edx*4]
        xor     edx, eax                              
        mov     eax,[esi + 0CH]
        dec     eax                                   
        mov    [esi + 00000C60H], edx      
        mov    [esi + 0CH], eax            
        mov     eax,[esi]                  
        mov     ebp,[eax + 10H]            
        inc     ebp                                   
        mov    [eax + 10H], ebp            
        mov     eax,[esi]                  
        mov     edi,[eax + 14H]
        dec     edi                                   
        mov    [eax + 14H], edi
        mov     eax,[esi]
        mov     edx,[eax + 18H]            
        inc     edx
        mov    [eax + 18H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 18H]            
        test    ecx, ecx                              
        jnz     @049                                 
        inc     dword ptr [eax + 1CH]                 
        jmp     @049                                 

@006:   push    esi
        mov     esi,[esp + 0CH]
        push    edi
        mov     edi,[esp + 0CH]
        xor     eax, eax
        mov     edx, 256
@007:   lea     ecx, [edx + eax]
        sar     ecx, 1
        cmp     edi,[esi + ecx*4]
        jl      @008
        mov     eax, ecx
        jmp     @009
@008:   mov     edx, ecx
@009:   mov     ecx, edx
        sub     ecx, eax
        cmp     ecx, 1
        jnz     @007
        pop     edi
        pop     esi
        ret     8
        
@050:   mov     eax,[esi + 0000FA40H]
        mov     ecx,[esi + 00000440H]
        inc     eax
        cmp     ecx, eax
        je      @062
        jg      @063
        mov     ecx,[esi + 38H]
        mov     al,[esi + 3CH]
        lea     edi, [esi + 00000444H]
        mov    [esi + 0CH], ebx
        push    edi
        push    ecx
        mov    [esi + 08H], al
        call    @006
        mov     edx,[esi + 38H]            
        mov     ebx,[esi + 00000C54H]      
        mov     ebp, edx                              
        xor     ecx, ecx                              
        shr     ebp, 1                                
        mov     cl,[ebx + ebp]              
        xor     ebp, ebp
        mov     ebx, ecx                              
        mov     cl, dl                                
        and     cl, 01H
        shl     cl, 2                                 
        shr     ebx, cl
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + edx*2]            
        mov     ecx,[esi + 14H]
        and     ebx, 0000000FH                        
        shl     ebx, 16
        or      ebx, ebp                              
        test    ecx, ecx                              
        mov    [esi + 38H], ebx            
        jnz     @051                                 
        mov     ecx,[esi + 18H]            
        mov     edx,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512                              
        mov    [esi + 14H], edx            
        mov    [esi + 18H], ecx            
        jnz     @051                                 
        mov     dword ptr [esi + 18H], 0
@051:   mov     ecx,[esi + 14H]            
        mov     ebx, 1                                
        dec     ecx                                   
        mov     edx,[esi + 0000FA40H]
        mov     ebp, ecx
        mov    [esi + 14H], ecx            
        cmp     ebp, ebx
        sete    cl                                    
        xor     al, cl                                
        mov    [esp + 14H], al              
        mov     eax,[esi + 00000440H]      
        inc     eax                                   
        inc     edx
        cmp     eax, edx                              
        mov    [esi + 00000440H], eax      
        je      @049                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx                              
        jz      @052
        mov    [esi + 3CH], eax            
        jmp     @049                                 

@052:   mov     eax,[esi + 38H]            
        push    edi
        push    eax                                   
        mov     dword ptr [esi + 0CH], 2              
        call    @006                                 
        mov     edx,[esi + 38H]            
        mov     ebx,[esi + 00000C54H]
        mov     ebp, edx                              
        xor     ecx, ecx                              
        shr     ebp, 1                                
        mov     cl,[ebx + ebp]              
        xor     ebp, ebp                              
        mov     ebx, ecx                              
        mov     cl, dl                                
        and     cl, 01H
        shl     cl, 2                                 
        shr     ebx, cl
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + edx*2]            
        mov     ecx,[esi + 14H]
        and     ebx, 0000000FH                        
        shl     ebx, 16                               
        or      ebx, ebp                              
        test    ecx, ecx                              
        mov    [esi + 38H], ebx            
        jnz     @053                                 
        mov     ecx,[esi + 18H]            
        mov     edx,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx
        cmp     ecx, 512                              
        mov    [esi + 14H], edx            
        mov    [esi + 18H], ecx            
        jnz     @053                                 
        mov     dword ptr [esi + 18H], 0              
@053:   mov     ecx,[esi + 14H]            
        mov     ebx, 1                                
        dec     ecx                                   
        mov     edx,[esi + 0000FA40H]
        mov     ebp, ecx
        mov    [esi + 14H], ecx            
        cmp     ebp, ebx
        sete    cl                                    
        xor     al, cl                                
        mov    [esp + 14H], al              
        mov     eax,[esi + 00000440H]      
        inc     eax                                   
        inc     edx
        cmp     eax, edx                              
        mov    [esi + 00000440H], eax      
        je      @049                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx                              
        jz      @054                                 
        mov    [esi + 3CH], eax            
        jmp     @049                                 

@054:   mov     eax,[esi + 38H]            
        push    edi
        push    eax                                   
        mov     dword ptr [esi + 0CH], 3              
        call    @006                                 
        mov     edx,[esi + 38H]            
        mov     ebx,[esi + 00000C54H]      
        mov     ebp, edx                              
        xor     ecx, ecx                              
        shr     ebp, 1                                
        mov     cl,[ebx + ebp]              
        xor     ebp, ebp
        mov     ebx, ecx                              
        mov     cl, dl                                
        and     cl, 01H
        shl     cl, 2                                 
        shr     ebx, cl
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + edx*2]            
        mov     ecx,[esi + 14H]
        and     ebx, 0000000FH                        
        shl     ebx, 16                               
        or      ebx, ebp                              
        test    ecx, ecx
        mov    [esi + 38H], ebx            
        jnz     @055                                 
        mov     ecx,[esi + 18H]            
        mov     edx,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512                              
        mov    [esi + 14H], edx            
        mov    [esi + 18H], ecx            
        jnz     @055                                 
        mov     dword ptr [esi + 18H], 0              
@055:   mov     ecx,[esi + 14H]            
        mov     ebx, 1                                
        dec     ecx                                   
        mov     edx,[esi + 0000FA40H]
        mov     ebp, ecx
        mov    [esi + 14H], ecx            
        cmp     ebp, ebx
        sete    cl                                    
        xor     al, cl                                
        mov    [esp + 14H], al              
        mov     eax,[esi + 00000440H]      
        inc     eax                                   
        inc     edx
        cmp     eax, edx                              
        mov    [esi + 00000440H], eax      
        je      @049                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx                              
        jz      @056
        mov    [esi + 3CH], eax            
        jmp     @049                                 

@056:   mov     eax,[esi + 38H]            
        push    edi
        push    eax                                   
        call    @006                                 
        mov     edx,[esi + 00000C54H]      
        xor     ecx, ecx                              
        mov    [esp + 14H], al
        mov     eax,[esi + 38H]            
        mov     ebp, eax                              
        shr     ebp, 1                                
        mov     cl,[edx + ebp]              
        xor     ebp, ebp                              
        mov     edx, ecx                              
        mov     cl, al                                
        and     cl, bl
        shl     cl, 2                                 
        shr     edx, cl
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + eax*2]
        mov     eax,[esi + 14H]
        and     edx, 0000000FH                        
        shl     edx, 16                               
        or      edx, ebp                              
        test    eax, eax                              
        mov    [esi + 38H], edx            
        jnz     @057                                 
        mov     eax,[esi + 18H]            
        mov     edx,dword ptr [_BZ2_rNums + eax*4]
        inc     eax                                   
        cmp     eax, 512                              
        mov    [esi + 14H], edx            
        mov    [esi + 18H], eax            
        jnz     @057                                 
        mov     dword ptr [esi + 18H], 0              
@057:   mov     eax,[esi + 14H]            
        mov     ebp,[esi + 00000440H]      
        mov     ecx,[esp + 14H]            
        dec     eax
        inc     ebp
        mov     edx,[esi + 38H]            
        cmp     eax, ebx
        mov    [esi + 14H], eax            
        sete    al                                    
        and     eax, 000000FFH                        
        and     ecx, 000000FFH                        
        xor     eax, ecx                              
        push    edi
        add     eax, 4                                
        push    edx                                   
        mov    [esi + 00000440H], ebp
        mov    [esi + 0CH], eax            
        call    @006                                 
        mov     edx,[esi + 38H]            
        mov     edi,[esi + 00000C54H]      
        mov     ebp, edx                              
        xor     ecx, ecx                              
        shr     ebp, 1                                
        mov    [esi + 3CH], eax            
        mov     cl,[edi + ebp]              
        xor     ebp, ebp
        mov     edi, ecx                              
        mov     cl, dl                                
        and     cl, bl                                
        shl     cl, 2                                 
        shr     edi, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + edx*2]            
        mov     ecx,[esi + 14H]            
        and     edi, 0000000FH                        
        shl     edi, 16                               
        or      edi, ebp                              
        test    ecx, ecx
        mov    [esi + 38H], edi
        jnz     @058                                 
        mov     ecx,[esi + 18H]
        mov     edx,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512
        mov    [esi + 14H], edx            
        mov    [esi + 18H], ecx            
        jnz     @058                                 
        mov     dword ptr [esi + 18H], 0              
@058:   mov     ebp,[esi + 14H]            
        xor     edx, edx                              
        dec     ebp                                   
        mov     ecx, ebp                              
        mov    [esi + 14H], ebp            
        cmp     ecx, ebx                              
        sete    dl                                    
        xor     edx, eax                              
        mov     eax,[esi + 00000440H]      
        inc     eax                                   
        mov    [esi + 3CH], edx            
        mov    [esi + 00000440H], eax      
        jmp     @049                                 

@059:   mov    [esi + 3CH], eax
@060:   mov     eax,[esi]                  
        mov     ecx,[eax + 14H]            
        test    ecx, ecx                              
        je      @062                                 
        mov     ecx,[esi + 0CH]            
        test    ecx, ecx
        jz      @061                                 
        mov     eax,[eax + 10H]
        mov     cl,[esi + 08H]              
        mov    [eax], cl                    
        mov     eax,[esi + 00000C60H]      
        mov     edx, eax                              
        xor     ecx, ecx                              
        mov     cl,[esi + 08H]              
        shr     edx, 24                               
        xor     edx, ecx
        shl     eax, 8                                
        mov     edx,dword ptr [_BZ2_crc32Table + edx*4]
        xor     edx, eax                              
        mov     eax,[esi + 0CH]            
        dec     eax
        mov    [esi + 00000C60H], edx      
        mov    [esi + 0CH], eax            
        mov     eax,[esi]                  
        mov     ebp,[eax + 10H]            
        inc     ebp                                   
        mov    [eax + 10H], ebp            
        mov     eax,[esi]
        mov     edi,[eax + 14H]            
        dec     edi                                   
        mov    [eax + 14H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 18H]            
        inc     edx
        mov    [eax + 18H], edx            
        mov     eax,[esi]
        mov     ecx,[eax + 18H]            
        test    ecx, ecx                              
        jnz     @060
        inc     dword ptr [eax + 1CH]                 
        jmp     @060                                 

@061:   mov     eax,[esi + 0000FA40H]      
        mov     ecx,[esi + 00000440H]      
        inc     eax                                   
        cmp     ecx, eax                              
        je      @062                                 
        jg      @063                                 
        mov     ecx,[esi + 38H]            
        mov     al,[esi + 3CH]              
        lea     edi, [esi + 00000444H]                
        mov    [esi + 0CH], ebx            
        push    edi                                   
        push    ecx                                   
        mov    [esi + 08H], al              
        call    @006                                 
        mov     ebp,[esi + 00000C54H]
        xor     ecx, ecx
        mov    [esp + 14H], al
        mov     eax,[esi + 38H]            
        mov     edx, eax                              
        shr     edx, 1                                
        mov     cl,[edx + ebp]              
        xor     ebp, ebp                              
        mov     edx, ecx
        mov     cl, al
        and     cl, bl                                
        shl     cl, 2                                 
        shr     edx, cl
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + eax*2]            
        mov     eax,[esi + 00000440H]      
        and     edx, 0000000FH                        
        shl     edx, 16                               
        or      edx, ebp                              
        inc     eax                                   
        mov    [esi + 38H], edx            
        mov     edx,[esi + 0000FA40H]      
        inc     edx
        mov    [esi + 00000440H], eax      
        cmp     eax, edx
        je      @060                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx                              
        jne     @059                                 
        mov     eax,[esi + 38H]            
        push    edi                                   
        push    eax                                   
        mov     dword ptr [esi + 0CH], 2              
        call    @006
        mov     edx,[esi + 00000C54H]      
        xor     ecx, ecx
        mov    [esp + 14H], al              
        mov     eax,[esi + 38H]            
        mov     ebp, eax
        shr     ebp, 1                                
        mov     cl,[edx + ebp]              
        xor     ebp, ebp                              
        mov     edx, ecx                              
        mov     cl, al                                
        and     cl, bl                                
        shl     cl, 2                                 
        shr     edx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + eax*2]            
        mov     eax,[esi + 00000440H]      
        and     edx, 0000000FH                        
        shl     edx, 16                               
        or      edx, ebp                              
        inc     eax
        mov    [esi + 38H], edx            
        mov     edx,[esi + 0000FA40H]      
        inc     edx
        mov    [esi + 00000440H], eax
        cmp     eax, edx                              
        je      @060                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx
        jne     @059                                 
        mov     eax,[esi + 38H]            
        push    edi                                   
        push    eax                                   
        mov     dword ptr [esi + 0CH], 3              
        call    @006                                 
        mov     edx,[esi + 00000C54H]      
        xor     ecx, ecx                              
        mov    [esp + 14H], al              
        mov     eax,[esi + 38H]            
        mov     ebp, eax                              
        shr     ebp, 1
        mov     cl,[edx + ebp]              
        xor     ebp, ebp                              
        mov     edx, ecx
        mov     cl, al                                
        and     cl, bl                                
        shl     cl, 2                                 
        shr     edx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + eax*2]            
        mov     eax,[esi + 00000440H]      
        and     edx, 0000000FH                        
        shl     edx, 16                               
        or      edx, ebp                              
        inc     eax                                   
        mov    [esi + 38H], edx            
        mov     edx,[esi + 0000FA40H]
        inc     edx                                   
        mov    [esi + 00000440H], eax
        cmp     eax, edx                              
        je      @060                                 
        mov     eax,[esp + 14H]
        mov     ecx,[esi + 3CH]            
        and     eax, 000000FFH                        
        cmp     eax, ecx
        jne     @059                                 
        mov     eax,[esi + 38H]            
        push    edi                                   
        push    eax                                   
        call    @006                                 
        mov     edx,[esi + 00000C54H]      
        xor     ecx, ecx                              
        mov    [esp + 14H], al              
        mov     eax,[esi + 38H]            
        mov     ebp, eax                              
        push    edi                                   
        shr     ebp, 1                                
        mov     cl,[edx + ebp]              
        xor     ebp, ebp                              
        mov     edx, ecx
        mov     cl, al
        and     cl, bl
        shl     cl, 2                                 
        shr     edx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     bp, word ptr [ecx + eax*2]            
        mov     ecx,[esi + 00000440H]      
        and     edx, 0000000FH                        
        shl     edx, 16                               
        or      edx, ebp                              
        inc     ecx                                   
        mov    [esi + 38H], edx            
        mov     edx,[esp + 18H]            
        mov     eax,[esi + 38H]
        and     edx, 000000FFH                        
        add     edx, 4                                
        push    eax                                   
        mov    [esi + 00000440H], ecx      
        mov    [esi + 0CH], edx            
        call    @006                                 
        mov     edx,[esi + 00000C54H]      
        mov    [esi + 3CH], eax
        mov     eax,[esi + 38H]            
        xor     ecx, ecx                              
        mov     edi, eax                              
        shr     edi, 1                                
        mov     cl,[edi + edx]              
        xor     edi, edi                              
        mov     edx, ecx                              
        mov     cl, al
        and     cl, bl                                
        shl     cl, 2                                 
        shr     edx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     di, word ptr [ecx + eax*2]
        mov     eax,[esi + 00000440H]      
        and     edx, 0000000FH
        shl     edx, 16
        or      edx, edi                              
        inc     eax
        mov    [esi + 38H], edx            
        mov    [esi + 00000440H], eax      
        jmp     @060                                 

@062:   pop     edi                                   
        pop     esi                                   
        pop     ebp                                   
        xor     al, al                                
        pop     ebx                                   
        ret 4

@063:   pop     edi
        pop     esi
        mov     al, bl
        pop     ebp
        pop     ebx
        ret 4    

@070:   sub     esp, 120                              
        push    ebx                                   
        push    ebp                                   
        push    esi                                   
        mov     esi,[esp + 00000088H]      
        push    edi                                   
        xor     edi, edi                              
        mov     eax,[esi + 04H]            
        mov     ebp,[esi]                  
        cmp     eax, 10                               
        jne     @071                                 
        mov    [esi + 0000FA14H], edi      
        mov    [esi + 0000FA18H], edi      
        mov    [esi + 0000FA1CH], edi      
        mov    [esi + 0000FA20H], edi      
        mov    [esi + 0000FA24H], edi      
        mov    [esi + 0000FA28H], edi      
        mov    [esi + 0000FA2CH], edi      
        mov    [esi + 0000FA30H], edi      
        mov    [esi + 0000FA34H], edi      
        mov    [esi + 0000FA38H], edi      
        mov    [esi + 0000FA3CH], edi
        mov    [esi + 0000FA40H], edi      
        mov    [esi + 0000FA44H], edi      
        mov    [esi + 0000FA48H], edi      
        mov    [esi + 0000FA4CH], edi      
        mov    [esi + 0000FA50H], edi      
        mov    [esi + 0000FA54H], edi      
        mov    [esi + 0000FA58H], edi      
        mov    [esi + 0000FA5CH], edi      
        mov    [esi + 0000FA60H], edi      
        mov    [esi + 0000FA64H], edi      
        mov    [esi + 0000FA68H], edi      
        mov    [esi + 0000FA6CH], edi      
        mov    [esi + 0000FA70H], edi
@071:   mov     eax,[esi + 0000FA14H]      
        mov     ecx,[esi + 0000FA18H]      
        mov     edx,[esi + 0000FA1CH]      
        mov    [esp + 14H], eax            
        mov     eax,[esi + 0000FA20H]
        mov    [esp + 1CH], ecx            
        mov     ecx,[esi + 0000FA24H]      
        mov    [esp + 4CH], eax            
        mov     eax,[esi + 0000FA2CH]      
        mov    [esp + 2CH], edx            
        mov     edx,[esi + 0000FA28H]      
        mov    [esp + 6CH], eax            
        mov     eax,[esi + 0000FA38H]      
        mov    [esp + 5CH], ecx            
        mov     ecx,[esi + 0000FA30H]      
        mov    [esp + 3CH], eax            
        mov     eax,[esi + 0000FA44H]
        mov    [esp + 34H], edx
        mov     edx,[esi + 0000FA34H]      
        mov    [esp + 50H], ecx            
        mov     ecx,[esi + 0000FA3CH]      
        mov    [esp + 20H], eax            
        mov     eax,[esi + 0000FA50H]
        mov    [esp + 38H], edx            
        mov     edx,[esi + 0000FA40H]      
        mov    [esp + 60H], ecx            
        mov     ecx,[esi + 0000FA48H]      
        mov    [esp + 00000084H], eax      
        mov     eax,[esi + 0000FA60H]      
        mov     ebx,[esi + 0000FA54H]      
        mov    [esp + 18H], edx            
        mov     edx,[esi + 0000FA4CH]      
        mov    [esp + 54H], ecx            
        mov     ecx,[esi + 0000FA58H]      
        mov    [esp + 68H], eax            
        mov     eax,[esi + 0000FA6CH]      
        mov    [esp + 30H], edx            
        mov     edx,[esi + 0000FA5CH]      
        mov    [esp + 24H], ecx            
        mov     ecx,[esi + 0000FA64H]      
        mov    [esp + 44H], eax            
        mov     eax,[esi + 04H]            
        mov    [esp + 64H], edx            
        mov     edx,[esi + 0000FA68H]      
        mov    [esp + 58H], ecx
        mov     ecx,[esi + 0000FA70H]      
        add     eax, -10                              
        mov    [esp + 10H], ebx            
        cmp     eax, 40                               
        mov    [esp + 40H], edx            
        mov    [esp + 48H], ecx            
        ja      @304                                 
        jmp     dword ptr [@306 + eax*4]

@072:   mov     dword ptr [esi + 04H], 10
@073:   mov     ecx,[esi + 20H]
        cmp     ecx, 8
        jge     @074
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx
        je      @303
        mov     edi,[eax]
        mov     edx,[esi + 1CH]
        xor     ebx, ebx
        add     ecx, 8
        mov     bl,[edi]
        mov    [esi + 20H], ecx
        shl     edx, 8
        or      edx, ebx
        mov    [esi + 1CH], edx
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @073                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @073                                 

@074:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 66                                
        jz      @076                                 
@075:   mov     ebx,[esp + 10H]            
        mov     edi, -5                               
        jmp     @305                                 

@076:   mov     dword ptr [esi + 04H], 11
@077:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @078                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx
        shl     edx, 8
        or      edx, ebx
        mov    [esi + 1CH], edx
        mov     ebx,[eax]
        inc     ebx
        mov    [eax], ebx
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @077                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @077                                 

@078:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 90                                
        jz      @079                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -5                               
        jmp     @305                                 

@079:   mov     dword ptr [esi + 04H], 12             
@080:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @081                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi
        mov     eax,[esi]
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @080                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @080                                 

@081:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 104                               
        jz      @082                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -5                               
        jmp     @305

@082:   mov     dword ptr [esi + 04H], 13             
@083:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @084                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @083                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @083

@084:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8
        shr     eax, cl                               
        mov    [esi + 20H], edx            
        and     eax, 000000FFH                        
        cmp     eax, 49                               
        mov    [esi + 24H], eax            
        jl      @075                                 
        cmp     eax, 57                               
        jg      @075                                 
        add     eax, -48                              
        mov     cl,[esi + 28H]              
        mov    [esi + 24H], eax            
        push    1                                     
        lea     eax, [eax + eax*4]                    
        test    cl, cl                                
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        jz      @085
        mov     ecx,[ebp + 2CH]            
        lea     eax, [eax + eax*4]                    
        shl     eax, 6                                
        push    eax                                   
        push    ecx                                   
        call    dword ptr [ebp + 24H]                 
        mov    [esi + 00000C50H], eax      
        mov     eax,[esi + 24H]            
        push    1                                     
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     edx, [eax + eax*4]                    
        mov     eax,[ebp + 2CH]            
        shl     edx, 5                                
        inc     edx                                   
        sar     edx, 1                                
        push    edx                                   
        push    eax                                   
        call    dword ptr [ebp + 24H]                 
        mov     ecx,[esi + 00000C50H]
        mov    [esi + 00000C54H], eax      
        test    ecx, ecx                              
        jz      @087
        jmp     @086                                 

@085:   mov     edx,[ebp + 2CH]            
        lea     ecx, [eax + eax*4]                    
        shl     ecx, 7                                
        push    ecx                                   
        push    edx                                   
        call    dword ptr [ebp + 24H]                 
        mov    [esi + 00000C4CH], eax      
@086:   test    eax, eax
        jnz     @088                                 
@087:   mov     ebx,[esp + 10H]            
        mov     edi, -3                               
        jmp     @305                                 

@088:   mov     dword ptr [esi + 04H], 14             
@089:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @090                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @089                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @089                                 

@090:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl
        mov    [esi + 20H], edx            
        and     eax, 000000FFH
        cmp     al, 23                                
        je      @274                                 
        cmp     al, 49                                
        jz      @091                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305                                 

@091:   mov     dword ptr [esi + 04H], 15             
@092:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8
        jge     @093                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @092                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @092                                 

@093:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 65                                
        jz      @094                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@094:   mov     dword ptr [esi + 04H], 16             
@095:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @096                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @095                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @095                                 

@096:   mov     edx,[esi + 20H]
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 89                                
        jz      @097                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@097:   mov     dword ptr [esi + 04H], 17             
@098:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @099                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @098                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @098                                 

@099:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 38                                
        jz      @100                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4
        jmp     @305

@100:   mov     dword ptr [esi + 04H], 18             
@101:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @102                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @101                                 
        inc     dword ptr [eax + 0CH]
        jmp     @101                                 

@102:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH
        cmp     al, 83                                
        jz      @103                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@103:   mov     dword ptr [esi + 04H], 19             
@104:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @105                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]
        test    ecx, ecx                              
        jnz     @104                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @104                                 

@105:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
//        and     eax, 000000FFH                        
        cmp     al, 89
        jz      @106                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@106:   mov     eax,[esi + 2CH]            
        mov     dword ptr [esi + 00000C58H], 0        
        inc     eax                                   
        mov    [esi + 2CH], eax
@107:   mov     dword ptr [esi + 04H], 20
@108:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @109                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @108                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @108                                 

@109:   mov     edx,[esi + 20H]
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C58H]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C58H], ecx      

@110:   mov     dword ptr [esi + 04H], 21
@111:   mov     ecx,[esi + 20H]
        cmp     ecx, 8
        jge     @112
        mov     eax,[esi]
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @111                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @111                                 

@112:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C58H]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax
        mov    [esi + 00000C58H], ecx      

@113:   mov     dword ptr [esi + 04H], 22
@114:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @115                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @114                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @114

@115:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C58H]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C58H], ecx      

@116:   mov     dword ptr [esi + 04H], 23
@117:   mov     ecx,[esi + 20H]
        cmp     ecx, 8
        jge     @118
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx
        je      @303
        mov     edi,[eax]
        mov     edx,[esi + 1CH]
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @117                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @117                                 

@118:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C58H]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C58H], ecx      

@119:   mov     dword ptr [esi + 04H], 24
@120:   mov     ecx,[esi + 20H]
        cmp     ecx, 1
        jge     @121
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx
        je      @303
        mov     edi,[eax]
        mov     edx,[esi + 1CH]
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx
        jnz     @120                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @120                                 

@121:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        mov     dword ptr [esi + 34H], 0              
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx                                   
        mov    [esi + 20H], edx            
        mov    [esi + 10H], al              

@122:
        mov     dword ptr [esi + 04H], 25             
@123:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @124                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]
        test    ecx, ecx                              
        jnz     @123                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @123                                 

@124:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 34H]            
        mov    [esi + 20H], edx            
        shl     ecx, 8
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 34H], ecx            

@125:   mov     dword ptr [esi + 04H], 26
@126:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @127                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @126                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @126                                 

@127:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl
        mov     ecx,[esi + 34H]            
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 34H], ecx            

@128:   mov     dword ptr [esi + 04H], 27
        xor     ebp, ebp                              
@129:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @130
        mov     eax,[esi]                  
        cmp    [eax + 04H], ebp            
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx
        mov     eax,[esi]                  
        cmp    [eax + 08H], ebp            
        jnz     @129                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @129                                 

@130:   mov     eax,[esi + 20H]            
        mov     edx,[esi + 1CH]            
        lea     ecx, [eax - 08H]                      
        add     eax, -8                               
        shr     edx, cl                               
        mov    [esi + 20H], eax            
        mov     eax,[esi + 34H]            
        shl     eax, 8                                
        and     edx, 000000FFH                        
        and     edx, 000000FFH                        
        or      eax, edx                              
        mov    [esi + 34H], eax            
        jge     @131                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@131:   mov     ecx,[esi + 24H]            
        lea     ecx, [ecx + ecx*4]                    
        lea     ecx, [ecx + ecx*4]                    
        lea     ecx, [ecx + ecx*4]                    
        lea     ecx, [ecx + ecx*4]                    
        lea     ecx, [ecx + ecx*4]                    
        shl     ecx, 5                                
        add     ecx, 10
        cmp     eax, ecx                              
        jle     @132                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4
        jmp     @305                                 

@132:   mov    [esp + 14H], ebp            
        jmp     @138                                 

@133:   mov     dword ptr [esi + 04H], 28
        xor     ebp, ebp                              
@134:   mov     ecx,[esi + 20H]            
        cmp     ecx, 1                                
        jge     @135                                 
        mov     eax,[esi]                  
        cmp    [eax + 04H], ebp            
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        cmp    [eax + 08H], ebp            
        jnz     @134                                 
        inc     dword ptr [eax + 0CH]
        jmp     @134                                 

@135:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx
        cmp     al, 1                                 
        mov    [esi + 20H], edx            
        jnz     @136                                 
        mov     eax,[esp + 14H]            
        mov     byte ptr [eax + esi + 00000D6CH], 1   
        jmp     @137                                 

@136:   mov     ecx,[esp + 14H]            
        mov     byte ptr [ecx + esi + 00000D6CH], 0
@137:   inc     dword ptr [esp + 14H]
@138:   mov     eax,[esp + 14H]
        mov     edx, 16
        cmp     eax, edx
        jl      @133
        lea     edi, [esi + 00000C6CH]
        mov     ecx, 64
        xor     eax, eax
        mov    [esp + 14H], ebp
        rep stosd
        jmp     @145

@139:   mov     dword ptr [esi + 04H], 29
        xor     ebp, ebp                              
@140:   mov     ecx,[esi + 20H]            
        cmp     ecx, 1                                
        jge     @141                                 
        mov     eax,[esi]                  
        cmp    [eax + 04H], ebp            
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        cmp    [eax + 08H], ebp            
        jnz     @140                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @140                                 

@141:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx                                   
        cmp     al, 1                                 
        mov    [esi + 20H], edx            
        jnz     @142                                 
        mov     eax,[esp + 14H]            
        mov     ecx,[esp + 1CH]            
        shl     eax, 4                                
        add     eax, ecx                              
        mov     byte ptr [eax + esi + 00000C6CH], 1   
@142:   mov     eax,[esp + 1CH]            
        mov     edx, 16
        inc     eax                                   
        mov    [esp + 1CH], eax            
@143:   cmp    [esp + 1CH], edx            
        jl      @139                                 
@144:   inc     dword ptr [esp + 14H]                 
@145:   mov     eax,[esp + 14H]            
        cmp     eax, edx                              
        jge     @146                                 
        mov     cl,[eax + esi + 00000D6CH]
        test    cl, cl                                
        jz      @144                                 
        mov    [esp + 1CH], ebp            
        jmp     @143                                 

@146:   push    esi                                   
        call    @307                                 
        mov     eax,[esi + 00000C68H]      
        cmp     eax, ebp                              
        jnz     @147                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305                                 

@147:   add     eax, 2
        mov    [esp + 4CH], eax

@148:   mov     dword ptr [esi + 04H], 30             
@149:   mov     ecx,[esi + 20H]            
        cmp     ecx, 3                                
        jge     @150                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @149
        inc     dword ptr [eax + 0CH]                 
        jmp     @149                                 

@150:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 03H]                      
        add     edx, -3                               
        shr     eax, cl                               
        mov    [esi + 20H], edx
        and     eax, 00000007H                        
        cmp     eax, 2                                
        mov    [esp + 5CH], eax            
        jl      @151                                 
        cmp     eax, 6                                
        jle     @152                                 
@151:   mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@152:   mov     dword ptr [esi + 04H], 31             
        xor     ebp, ebp                              
@153:   mov     ecx,[esi + 20H]            
        cmp     ecx, 15
        jge     @154                                 
        mov     eax,[esi]                  
        cmp    [eax + 04H], ebp            
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        cmp    [eax + 08H], ebp            
        jnz     @153                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @153                                 

@154:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 0FH]                      
        add     edx, -15                              
        shr     eax, cl
        mov    [esi + 20H], edx            
        and     eax, 00007FFFH                        
        mov     edi, eax                              
        cmp     eax, 1                                
        mov    [esp + 34H], edi            
        jge     @155                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@155:   mov    [esp + 14H], ebp
        jmp     @160

@156:   mov     dword ptr [esi + 04H], 32             
        xor     ebp, ebp                              
@157:   mov     ecx,[esi + 20H]            
        cmp     ecx, 1                                
        jge     @158                                 
        mov     eax,[esi]                  
        cmp    [eax + 04H], ebp            
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        cmp    [eax + 08H], ebp            
        jnz     @157                                 
        inc     dword ptr [eax + 0CH]
        jmp     @157                                 

@158:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx                                   
        test    al, al                                
        mov    [esi + 20H], edx            
        jz      @159                                 
        mov     eax,[esp + 1CH]            
        mov     ecx,[esp + 5CH]
        inc     eax                                   
        cmp     eax, ecx                              
        mov    [esp + 1CH], eax            
        jl      @156                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@159:   mov     eax,[esp + 14H]
        mov     cl,[esp + 1CH]
        mov     edi,[esp + 34H]
        mov    [eax + esi + 0000650EH], cl  
        inc     eax                                   
        mov    [esp + 14H], eax            
@160:   cmp    [esp + 14H], edi            
        jge     @161                                 
        mov    [esp + 1CH], ebp            
        jmp     @156                                 

@161:   mov     edx,[esp + 5CH]            
        xor     cl, cl                                
        cmp     edx, ebp                              
        jle     @163
        xor     eax, eax                              
@162:   mov    [esp + eax + 7CH], cl        
        inc     cl                                    
        mov    [esp + 2CH], cl              
        mov     eax,[esp + 2CH]            
        and     eax, 000000FFH                        
        cmp     eax, edx                              
        jl      @162                                 
@163:   cmp     edi, ebp                              
        mov    [esp + 14H], ebp            
        jle     @167                                 
        mov     ebp,[esp + 34H]            
@164:   mov     edx,[esp + 14H]            
        mov     dl,[esi + edx + 0000650EH]  
        mov    [esp + 2CH], dl              
        mov     eax,[esp + 2CH]            
        and     eax, 000000FFH                        
        test    dl, dl                                
        mov     bl,[esp + eax + 7CH]        
        lea     ecx, [esp + eax + 7CH]                
        jbe     @166                                 
        mov     edi, eax
@165:   mov     al,[ecx - 01H]              
        mov    [ecx], al                    
        dec     ecx                                   
        dec     edi                                   
        jnz     @165                                 
@166:   mov     eax,[esp + 14H]            
        mov    [esp + 7CH], bl              
        mov    [eax + esi + 00001EBCH], bl  
        inc     eax                                   
        cmp     eax, ebp                              
        mov    [esp + 14H], eax            
        jl      @164                                 
@167:   mov     dword ptr [esp + 2CH], 0
        jmp     @178                                 

@168:   mov     dword ptr [esi + 04H], 35
@169:   mov     ecx,[esi + 20H]
        cmp     ecx, 1
        jge     @170
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @169                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @169                                 

@170:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx                                   
        test    al, al                                
        mov     eax,[esp + 30H]
        mov    [esi + 20H], edx            
        jnz     @171                                 
        inc     eax                                   
        jmp     @172                                 

@171:  dec     eax                                   
@172:   mov    [esp + 30H], eax            
@173:   mov     eax,[esp + 30H]            
        cmp     eax, 1                                
        jl      @289                                 
        cmp     eax, 20                               
        jg      @289                                 

@174:   mov     dword ptr [esi + 04H], 34
@175:   mov     ecx,[esi + 20H]
        cmp     ecx, 1
        jge     @176                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @175                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @175                                 

@176:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        and     eax, 00000001H                        
        dec     edx                                   
        test    al, al                                
        mov    [esi + 20H], edx            
        jne     @168                                 
        mov     eax,[esp + 2CH]
        mov     ecx, eax                              
        shl     ecx, 7                                
        add     ecx, eax                              
        mov     eax,[esp + 14H]            
        lea     edx, [eax + ecx*2]                    
        mov     cl,[esp + 30H]              
        inc     eax                                   
        mov    [edx + esi + 0000AB60H], cl  
        mov    [esp + 14H], eax            
@177:   mov     edx,[esp + 14H]            
        mov     eax,[esp + 4CH]            
        cmp     edx, eax                              
        jl      @173
        inc     dword ptr [esp + 2CH]
@178:   mov     eax,[esp + 5CH]
        mov     ecx,[esp + 2CH]
        cmp     ecx, eax
        jge     @182

@179:   mov     dword ptr [esi + 04H], 33
@180:   mov     ecx,[esi + 20H]            
        cmp     ecx, 5                                
        jge     @181                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @180
        inc     dword ptr [eax + 0CH]                 
        jmp     @180                                 

@181:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        mov     dword ptr [esp + 14H], 0              
        lea     ecx, [edx - 05H]
        shr     eax, cl                               
        and     eax, 0000001FH                        
        add     edx, -5                               
        mov    [esi + 20H], edx            
        mov    [esp + 30H], eax            
        jmp     @177                                 

@182:   test    eax, eax                              
        mov     dword ptr [esp + 2CH], 0              
        jle     @188                                 
        lea     ecx, [esi + 0000F9FCH]                
        lea     edi, [esi + 0000C99CH]                
        mov    [esp + 6CH], ecx
        lea     ebx, [esi + 0000AB60H]                
        mov    [esp + 60H], eax            
        mov    [esp + 2CH], eax            
@183:   mov     eax,[esp + 4CH]            
        xor     ecx, ecx                              
        xor     edx, edx                              
        mov     ebp, 32
        test    eax, eax                              
        jle     @187                                 
@184:   xor     eax, eax                              
        mov     al,[ebx + edx]              
        cmp     eax, ecx                              
        jle     @185                                 
        mov     ecx, eax                              
@185:   cmp     eax, ebp                              
        jge     @186                                 
        mov     ebp, eax                              
@186:   mov     eax,[esp + 4CH]            
        inc     edx                                   
        cmp     edx, eax                              
        jl      @184                                 
@187:   mov     edx,[esp + 4CH]            
        lea     eax, [edi + 00001830H]
        push    edx                                   
        push    ecx                                   
        push    ebp                                   
        push    ebx                                   
        push    eax                                   
        lea     ecx, [edi - 00001830H]                
        push    edi                                   
        push    ecx                                   
        call    @310                                 
        add     ebx, 258
        add     edi, 1032                             
        mov     eax,[esp + 6CH]            
        mov    [eax], ebp                  
        add     eax, 4                                
        mov    [esp + 6CH], eax
        mov     eax,[esp + 60H]            
        dec     eax                                   
        mov    [esp + 60H], eax            
        jnz     @183                                 
@188:   mov     eax,[esi + 00000C68H]      
        xor     ebp, ebp                              
        inc     eax
        lea     edi, [esi + 40H]                      
        mov    [esp + 6CH], eax            
        mov     eax,[esi + 24H]            
        mov     ecx, 256                              
        mov     edx, 15                               
        lea     eax, [eax + eax*4]                    
        mov    [esp + 38H], ebp            
        mov     dword ptr [esp + 14H], 256            
        mov    [esp + 50H], edx            
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]                    
        lea     eax, [eax + eax*4]
        shl     eax, 5                                
        mov    [esp + 60H], eax            
        xor     eax, eax                              
        rep stosd                                     
        mov     eax, 4095                             
        lea     edi, [esi + 00001EB8H]                
@189:   mov     ecx, 15
        shl     dl, 4                                 
@190:   mov     bl, cl                                
        add     bl, dl                                
        mov    [eax + esi + 00000E7CH], bl  
        dec     eax                                   
        dec     ecx                                   
        jns     @190                                 
        lea     edx, [eax + 01H]                      
        mov    [edi], edx                  
        mov     edx,[esp + 50H]            
        dec     edx                                   
        sub     edi, 4                                
        cmp     edx, ebp                              
        mov    [esp + 50H], edx            
        jge     @189                                 
        mov     eax,[esp + 34H]
        mov    [esp + 18H], ebp            
        cmp     eax, ebp                              
        mov    [esp + 50H], ebp            
        jg      @191                                 
        mov     ebx,[esp + 10H]
        mov     edi, -4                               
        jmp     @305

@191:   xor     eax, eax
        mov     dword ptr [esp + 38H], 49
        mov     al,[esi + 00001EBCH]
        mov     edx, eax
        mov    [esp + 68H], eax
        mov     ecx,[esi + eax*4 + 0000F9FCH]
        shl     edx, 7
        add     edx, eax
        mov    [esp + 58H], ecx
        mov    [esp + 10H], ecx
        lea     eax, [esi + edx*8]
        lea     edx, [eax + 0000B16CH]
        mov    [esp + 40H], edx
        lea     edx, [eax + 0000E1CCH]
        add     eax, 51612
        mov    [esp + 48H], edx
        mov    [esp + 44H], eax
@192:   mov     ebx,[esp + 10H]
        mov     dword ptr [esi + 04H], 36             
@193:   mov     ecx,[esi + 20H]            
        cmp     ecx, ebx                              
        jge     @194                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @273                                 
        mov     edi,[eax]
        mov     ebp,[esi + 1CH]            
        xor     edx, edx                              
        add     ecx, 8                                
        mov     dl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     ebp, 8                                
        or      ebp, edx
        mov    [esi + 1CH], ebp            
        mov     ebp,[eax]                  
        inc     ebp                                   
        mov    [eax], ebp                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]
        test    ecx, ecx                              
        jnz     @193
        inc     dword ptr [eax + 0CH]                 
        jmp     @193                                 

@194:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        sub     edx, ebx                              
        mov     edi, 1                                
        mov     ecx, edx                              
        mov    [esi + 20H], edx            
        shr     eax, cl                               
        mov     ecx, ebx                              
        shl     edi, cl                               
        dec     edi                                   
        and     eax, edi                              
        mov    [esp + 24H], eax            
        jmp     @199                                 

@195:   mov     ebx,[esp + 10H]
@196:   mov     dword ptr [esi + 04H], 37
@197:   mov     ecx,[esi + 20H]
        cmp     ecx, 1
        jge     @198
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx
        je      @273
        mov     edi,[eax]
        mov     ebp,[esi + 1CH]
        xor     edx, edx
        add     ecx, 8
        mov     dl,[edi]
        mov    [esi + 20H], ecx
        shl     ebp, 8
        or      ebp, edx
        mov    [esi + 1CH], ebp
        mov     ebp,[eax]
        inc     ebp
        mov    [eax], ebp
        mov     eax,[esi]
        mov     edi,[eax + 04H]
        dec     edi
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @197
        inc     dword ptr [eax + 0CH]                 
        jmp     @197                                 

@198:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        mov     ecx,[esp + 24H]
        and     eax, 00000001H                        
        dec     edx                                   
        mov    [esi + 20H], edx            
        lea     edx, [ecx + ecx]                      
        or      edx, eax                              
        mov    [esp + 64H], eax            
        mov    [esp + 24H], edx            
        mov     eax, edx                              
@199:   cmp     ebx, 20                               
        jle     @200                                 
        mov     edi, -4                               
        jmp     @305

@200:   mov     ecx,[esp + 40H]
        cmp     eax,[ecx + ebx*4]
        jle     @201
        inc     ebx
        mov    [esp + 10H], ebx
        jmp     @196

@201:   mov     edx,[esp + 44H]
        sub     eax,[edx + ebx*4]
        js      @202
        cmp     eax, 258
        jl      @210
@202:   mov     edi, -4
        jmp     @305

@203:   mov     ebx,[esp + 10H]            
@204:   mov     dword ptr [esi + 04H], 41             
@205:   mov     ecx,[esi + 20H]            
        cmp     ecx, 1                                
        jge     @206                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]
        test    edx, edx                              
        je      @273                                 
        mov     edi,[eax]                  
        mov     ebp,[esi + 1CH]            
        xor     edx, edx                              
        add     ecx, 8                                
        mov     dl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     ebp, 8
        or      ebp, edx                              
        mov    [esi + 1CH], ebp            
        mov     ebp,[eax]                  
        inc     ebp                                   
        mov    [eax], ebp
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @205                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @205                                 

@206:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        mov     ecx,[esp + 24H]            
        and     eax, 00000001H                        
        dec     edx                                   
        mov    [esi + 20H], edx            
        lea     edx, [ecx + ecx]                      
        mov    [esp + 64H], eax            
        or      eax, edx                              
        mov    [esp + 24H], eax
@207:   cmp     ebx, 20                               
        jle     @208                                 
        mov     edi, -4                               
        jmp     @305

@208:   mov     ecx,[esp + 40H]            
        cmp     eax,[ecx + ebx*4]          
        jle     @209                                 
        inc     ebx                                   
        mov    [esp + 10H], ebx            
        jmp     @204                                 

@209:   mov     edx,[esp + 44H]            
        sub     eax,[edx + ebx*4]          
        js      @260                                 
        cmp     eax, 258                              
        jge     @260
@210:   mov     ecx,[esp + 48H]            
        mov     edx,[ecx + eax*4]          
        mov    [esp + 3CH], edx            
@211:   mov     ecx,[esp + 3CH]
        mov     eax,[esp + 6CH]            
        cmp     ecx, eax                              
        je      @253                                 
        test    ecx, ecx                              
        je      @232                                 
        cmp     ecx, 1                                
        je      @232                                 
        mov     eax,[esp + 60H]            
        mov     edx,[esp + 18H]            
        cmp     edx, eax                              
        jl      @212                                 
        mov     edi, -4                               
        jmp     @305

@212:   dec     ecx                                   
        cmp     ecx, 16                               
        jae     @217                                 
        mov     edi,[esi + 00001E7CH]      
        cmp     ecx, 3
        mov    [esp + 7CH], edi            
        lea     eax, [edi + ecx]                      
        mov     dl,[eax + esi + 00000E7CH]  
        mov    [esp + 28H], dl              
        jbe     @214                                 
        lea     edx, [esi + 00000E7BH]                
        mov     edi, esi                              
        sub     edi, edx                              
        add     eax, edx                              
        add     edi, 3708                             
        mov    [esp + 70H], edi            
        mov     edi, esi                              
        sub     edi, edx                              
        lea     ebp, [edi + 00000E7AH]                
        mov     edi, esi                              
        sub     edi, edx                              
        add     edi, 3704                             
        mov    [esp + 78H], edi
        mov     edi, esi                              
        sub     edi, edx                              
        add     edi, 3705                             
        mov    [esp + 74H], edi
        mov     edi, ecx                              
        shr     edi, 2                                
@213:   mov     ebx,[esp + 70H]            
        mov     dl,[eax]                    
        sub     ecx, 4                                
        mov    [ebx + eax], dl              
        mov     dl,[eax + ebp]              
        mov    [eax], dl                    
        mov     edx,[esp + 74H]            
        mov     bl,[edx + eax]              
        mov    [eax + ebp], bl              
        mov     ebx,[esp + 78H]
        mov     bl,[ebx + eax]
        mov    [edx + eax], bl              
        sub     eax, 4                                
        dec     edi                                   
        jnz     @213                                 
        mov     edi,[esp + 7CH]            
@214:   test    ecx, ecx                              
        jbe     @216                                 
        lea     eax, [edi + esi + 00000E7CH]          
@215:   mov     dl,[ecx + eax - 01H]        
        mov    [ecx + eax], dl              
        dec     ecx                                   
        jnz     @215                                 
@216:   mov     al,[esp + 28H]              
        mov    [edi + esi + 00000E7CH], al  
        jmp     @224                                 

@217:   mov     edx, ecx                              
        and     ecx, 0000000FH                        
        shr     edx, 4                                
        mov     ebp,[esi + edx*4 + 00001E7CH]
        lea     edi, [esi + edx*4 + 00001E7CH]        
        add     ecx, ebp                              
        cmp     ecx, ebp
        mov     bl,[ecx + esi + 00000E7CH]  
        mov    [esp + 28H], bl              
        jle     @219                                 
@218:   mov     al,[ecx + esi + 00000E7BH]  
        mov    [ecx + esi + 00000E7CH], al  
        mov     eax,[edi]                  
        dec     ecx                                   
        cmp     ecx, eax                              
        jg      @218                                 
@219:   mov     ecx,[edi]                  
        inc     ecx                                   
        test    edx, edx                              
        mov    [edi], ecx                  
        jle     @221                                 
        mov     eax, edi                              
        mov     edi, edx                              
@220:   mov     edx,[eax]                  
        add     eax, -4
        dec     edx                                   
        dec     edi                                   
        mov    [eax + 04H], edx            
        mov     ecx, edx
        mov     edx,[eax]                  
        mov     dl,[edx + esi + 00000E8BH]  
        mov    [ecx + esi + 00000E7CH], dl  
        jnz     @220                                 
@221:   mov     ecx,[esi + 00001E7CH]      
        dec     ecx                                   
        mov     eax, ecx
        mov    [esi + 00001E7CH], ecx      
        mov    [eax + esi + 00000E7CH], bl  
        mov     eax,[esi + 00001E7CH]      
        test    eax, eax                              
        jnz     @224                                 
        mov     eax, 4095
        lea     ecx, [esi + 00001EB8H]                
        mov     ebp, 16                               
@222:   mov     edi, 15                               
@223:   mov     ebx,[ecx]                  
        mov     edx, edi                              
        add     edx, ebx                              
        dec     eax                                   
        dec     edi                                   
        mov     dl,[edx + esi + 00000E7CH]  
        mov    [eax + esi + 00000E7DH], dl  
        jns     @223                                 
        lea     edx, [eax + 01H]                      
        mov    [ecx], edx                  
        sub     ecx, 4                                
        dec     ebp                                   
        jnz     @222                                 
@224:   mov     eax,[esp + 28H]            
        xor     ecx, ecx                              
        and     eax, 000000FFH                        
        mov     cl,[eax + esi + 00000D7CH]  
        lea     eax, [eax + esi + 00000D7CH]          
        mov     edx,[esi + ecx*4 + 40H]    
        lea     ecx, [esi + ecx*4 + 40H]
        inc     edx                                   
        mov    [ecx], edx                  
        mov     cl,[esi + 28H]              
        test    cl, cl                                
        jz      @225                                 
        movzx   dx, byte ptr [eax]                    
        mov     eax,[esi + 00000C50H]      
        mov     ecx,[esp + 18H]            
        mov     word ptr [eax + ecx*2], dx            
        jmp     @226                                 

@225:   mov     ecx,[esp + 18H]            
        xor     edx, edx                              
        mov     dl,[eax]                    
        mov     eax,[esi + 00000C4CH]      
        mov    [eax + ecx*4], edx          
@226:   mov     ecx,[esp + 18H]            
        inc     ecx
        mov    [esp + 18H], ecx            
        mov     ecx,[esp + 38H]            
        test    ecx, ecx                              
        jnz     @228
        mov     eax,[esp + 50H]            
        mov     ecx,[esp + 34H]
        inc     eax                                   
        cmp     eax, ecx                              
        mov    [esp + 50H], eax            
        jl      @227                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@227:   xor     edx, edx
        mov     ecx, 50
        mov     dl,[eax + esi + 00001EBCH]
        mov     eax, edx
        mov    [esp + 68H], eax
        mov     edx,[esi + eax*4 + 0000F9FCH]
        mov    [esp + 58H], edx
        mov     edx, eax
        shl     edx, 7
        add     edx, eax
        lea     eax, [esi + edx*8]
        lea     edx, [eax + 0000B16CH]
        mov    [esp + 40H], edx
        lea     edx, [eax + 0000E1CCH]
        add     eax, 51612
        mov    [esp + 48H], edx
        mov    [esp + 44H], eax
@228:   mov     eax,[esp + 58H]
        dec     ecx
        mov    [esp + 38H], ecx
        mov    [esp + 10H], eax
@229:   mov     ebx,[esp + 10H]
        mov     dword ptr [esi + 04H], 40             
@230:   mov     ecx,[esi + 20H]            
        cmp     ecx, ebx                              
        jge     @231
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @273                                 
        mov     edi,[eax]                  
        mov     ebp,[esi + 1CH]            
        xor     edx, edx                              
        add     ecx, 8                                
        mov     dl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     ebp, 8                                
        or      ebp, edx                              
        mov    [esi + 1CH], ebp            
        mov     ebp,[eax]                  
        inc     ebp                                   
        mov    [eax], ebp                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @230                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @230                                 

@231:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        sub     edx, ebx                              
        mov     edi, 1
        mov     ecx, edx                              
        mov    [esi + 20H], edx            
        shr     eax, cl                               
        mov     ecx, ebx                              
        shl     edi, cl                               
        dec     edi                                   
        and     eax, edi                              
        mov    [esp + 24H], eax            
        jmp     @207                                 

@232:   mov     dword ptr [esp + 20H], -1             
        mov     dword ptr [esp + 54H], 1              
@233:   mov     eax,[esp + 3CH]            
        test    eax, eax                              
        jnz     @234                                 
        mov     eax,[esp + 54H]            
        mov     ecx,[esp + 20H]            
        add     ecx, eax                              
        mov    [esp + 20H], ecx            
        jmp     @235                                 

@234:   cmp     eax, 1                                
        jnz     @235
        mov     ecx,[esp + 20H]            
        mov     edx,[esp + 54H]            
        lea     eax, [ecx + edx*2]                    
        mov    [esp + 20H], eax            
@235:   mov     ecx,[esp + 54H]            
        lea     edx, [ecx + ecx]                      
        mov     ecx,[esp + 38H]            
        test    ecx, ecx                              
        mov    [esp + 54H], edx            
        jnz     @237                                 
        mov     eax,[esp + 50H]            
        mov     ecx,[esp + 34H]
        inc     eax                                   
        cmp     eax, ecx                              
        mov    [esp + 50H], eax            
        jl      @236                                 
        mov     edi, -4                               
        jmp     @305

@236:   xor     edx, edx
        mov     ecx, 50
        mov     dl,[eax + esi + 00001EBCH]
        mov     eax, edx
        mov    [esp + 68H], eax
        mov     edx,[esi + eax*4 + 0000F9FCH]
        mov    [esp + 58H], edx
        mov     edx, eax
        shl     edx, 7
        add     edx, eax
        lea     eax, [esi + edx*8]
        lea     edx, [eax + 0000B16CH]
        mov    [esp + 40H], edx
        lea     edx, [eax + 0000E1CCH]
        add     eax, 51612
        mov    [esp + 48H], edx
        mov    [esp + 44H], eax
@237:   mov     ebx,[esp + 58H]
        dec     ecx
        mov    [esp + 38H], ecx
        mov    [esp + 10H], ebx
        jmp     @239

@238:   mov     ebx,[esp + 10H]            
@239:   mov     dword ptr [esi + 04H], 38             
@240:   mov     ecx,[esi + 20H]            
        cmp     ecx, ebx                              
        jge     @241                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @273                                 
        mov     edi,[eax]                  
        mov     ebp,[esi + 1CH]            
        xor     edx, edx                              
        add     ecx, 8                                
        mov     dl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     ebp, 8
        or      ebp, edx                              
        mov    [esi + 1CH], ebp            
        mov     ebp,[eax]                  
        inc     ebp                                   
        mov    [eax], ebp                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @240                                 
        inc     dword ptr [eax + 0CH]
        jmp     @240                                 

@241:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]
        sub     edx, ebx                              
        mov     edi, 1                                
        mov     ecx, edx                              
        mov    [esi + 20H], edx            
        shr     eax, cl                               
        mov     ecx, ebx                              
        shl     edi, cl                               
        dec     edi                                   
        and     eax, edi                              
        mov    [esp + 24H], eax            
@242:   cmp     ebx, 20                               
        jle     @243                                 
        mov     edi, -4
        jmp     @305

@243:   mov     ecx,[esp + 40H]
        cmp     eax,[ecx + ebx*4]
        jle     @248
        inc     ebx
        mov    [esp + 10H], ebx
        jmp     @245

@244:   mov     ebx,[esp + 10H]            
@245:   mov     dword ptr [esi + 04H], 39             
@246:   mov     ecx,[esi + 20H]            
        cmp     ecx, 1                                
        jge     @247                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @273                                 
        mov     edi,[eax]                  
        mov     ebp,[esi + 1CH]            
        xor     edx, edx                              
        add     ecx, 8                                
        mov     dl,[edi]
        mov    [esi + 20H], ecx            
        shl     ebp, 8                                
        or      ebp, edx                              
        mov    [esi + 1CH], ebp            
        mov     ebp,[eax]                  
        inc     ebp                                   
        mov    [eax], ebp                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx
        jnz     @246                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @246                                 

@247:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 01H]                      
        shr     eax, cl                               
        mov     ecx,[esp + 24H]            
        and     eax, 00000001H                        
        dec     edx                                   
        mov    [esi + 20H], edx            
        lea     edx, [ecx + ecx]                      
        or      edx, eax                              
        mov    [esp + 64H], eax            
        mov    [esp + 24H], edx            
        mov     eax, edx
        jmp     @242                                 

@248:   mov     ecx,[esp + 44H]            
        sub     eax,[ecx + ebx*4]          
        js      @260                                 
        cmp     eax, 258                              
        jge     @260                                 
        mov     edx,[esp + 48H]            
        mov     eax,[edx + eax*4]          
        test    eax, eax                              
        mov    [esp + 3CH], eax            
        je      @233                                 
        cmp     eax, 1                                
        je      @233                                 
        mov     eax,[esi + 00001E7CH]      
        xor     ecx, ecx                              
        mov     edx,[esp + 20H]            
        mov     cl,[eax + esi + 00000E7CH]
        inc     edx                                   
        mov    [esp + 20H], edx            
        mov     cl,[ecx + esi + 00000D7CH]  
        mov    [esp + 28H], cl              
        mov     eax,[esp + 28H]
        and     eax, 000000FFH                        
        mov     ebp,[esi + eax*4 + 40H]    
        add     ebp, edx                              
        mov    [esi + eax*4 + 40H], ebp    
        cmp     byte ptr [esi + 28H], 0               
        jz      @251                                 
        test    edx, edx                              
        jle     @211                                 
        mov     eax,[esp + 18H]            
@249:   cmp     eax,[esp + 60H]            
        jge     @250                                 
        mov     ebp,[esi + 00000C50H]      
        inc     eax                                   
        movzx   di, cl                                
        mov     word ptr [ebp + eax*2 - 02H], di      
        dec     edx                                   
        test    edx, edx                              
        jg      @249
        mov    [esp + 20H], edx            
        mov    [esp + 18H], eax            
        jmp     @211                                 

@250:   mov    [esp + 20H], edx            
        mov    [esp + 18H], eax            
        mov     edi, -4                               
        jmp     @305                                 

@251:   test    edx, edx                              
        jle     @211                                 
        mov     edi,[esp + 60H]            
        mov     ecx,[esp + 18H]            
        mov     edx,[esp + 20H]            
@252:   cmp     ecx, edi                              
        jge     @260                                 
        mov     ebp,[esi + 00000C4CH]
        inc     ecx                                   
        dec     edx                                   
        mov    [esp + 18H], ecx            
        mov    [ebp + ecx*4 - 04H], eax    
        mov    [esp + 20H], edx            
        test    edx, edx                              
        jg      @252                                 
        jmp     @211                                 

@253:   mov     eax,[esi + 34H]            
        test    eax, eax                              
        jl      @260                                 
        mov     edi,[esp + 18H]
        cmp     eax, edi                              
        jge     @260                                 
        lea     eax, [esi + 00000448H]                
        mov     dword ptr [esi + 00000444H], 0        
        mov     ecx, eax                              
        mov     edx, 256                              
@254:   mov     ebp,[ecx - 00000408H]      
        mov    [ecx], ebp                  
        add     ecx, 4                                
        dec     edx
        jnz     @254                                 
        mov     ecx, 256                              
@255:   mov     edx,[eax - 04H]            
        mov     ebp,[eax]                  
        add     ebp, edx                              
        mov    [eax], ebp                  
        add     eax, 4                                
        dec     ecx                                   
        jnz     @255                                 
        xor     ebp, ebp                              
        xor     eax, eax                              
        mov    [esp + 14H], eax            
        lea     ecx, [esi + 00000444H]                
@256:   mov     edx,[ecx]                  
        cmp     edx, ebp                              
        jl      @259                                 
        cmp     edx, edi                              
        jg      @259
        inc     eax                                   
        add     ecx, 4                                
        cmp     eax, 256                              
        jle     @256
        mov    [esp + 14H], eax            
        mov     al,[esi + 28H]              
        test    al, al                                
        mov    [esi + 0CH], ebp            
        mov     byte ptr [esi + 08H], 0               
        mov     dword ptr [esi + 00000C60H], -1       
        mov     dword ptr [esi + 04H], 2              
        je      @269                                 
        lea     eax, [esi + 00000848H]                
        mov     ecx, 257                              
@257:   mov     edx,[eax - 00000404H]      
        mov    [eax], edx                  
        add     eax, 4
        dec     ecx                                   
        jnz     @257                                 
        xor     eax, eax                              
        cmp     edi, ebp                              
        jle     @263                                 
@258:   mov     ecx,[esi + 00000C50H]      
        mov     dl,[ecx + eax*2]            
        lea     ecx, [ecx + eax*2]
        mov    [esp + 28H], dl              
        mov     edx,[esp + 28H]            
        and     edx, 000000FFH                        
        test    al, 01H                               
        lea     edi, [esi + edx*4 + 00000848H]        
        mov     dx, word ptr [esi + edx*4 + 00000848H]
        mov     word ptr [ecx], dx                    
        mov     ecx, eax                              
        jnz     @261                                 
        mov     edx,[esi + 00000C54H]      
        sar     ecx, 1                                
        lea     ebp, [edx + ecx]                      
        mov     ecx,[edi]                  
        sar     ecx, 16                               
        mov     dl,[ebp]
        and     dl, 0F0H                              
        or      cl, dl                                
        mov    [ebp], cl                    
        jmp     @262                                 

@259:   mov    [esp + 14H], eax            
@260:   mov     edi, -4                               
        jmp     @305                                 

@261:   mov     edx,[esi + 00000C54H]      
        sar     ecx, 1                                
        mov     bl,[edx + ecx]              
        lea     ebp, [edx + ecx]                      
        mov     ecx,[edi]                  
        sar     ecx, 12                               
        mov     dl, cl                                
        xor     dl, bl                                
        and     dl, 0FH
        xor     dl, cl                                
        mov    [ebp], dl                    
@262:   mov     ebx,[edi]                  
        mov     ecx,[esp + 18H]
        inc     ebx                                   
        inc     eax                                   
        cmp     eax, ecx                              
        mov    [edi], ebx                  
        jl      @258                                 
@263:   mov     edx,[esi + 34H]            
        mov     ecx,[esi + 00000C54H]      
        mov     eax, edx                              
        xor     ebx, ebx                              
        sar     eax, 1                                
        xor     edi, edi                              
        mov    [esp + 14H], edx            
        mov     bl,[eax + ecx]
        mov     cl, dl                                
        and     cl, 01H                               
        mov     eax, ebx
        shl     cl, 2                                 
        shr     eax, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     di, word ptr [ecx + edx*2]            
        and     eax, 0000000FH                        
        shl     eax, 16                               
        or      eax, edi                              
        mov    [esp + 1CH], eax            
@264:   mov     eax,[esp + 1CH]            
        mov     ecx,[esi + 00000C50H]      
        mov     edx, eax                              
        mov     ebx, eax                              
        lea     edi, [ecx + eax*2]                    
        mov     eax,[esi + 00000C54H]      
        sar     edx, 1                                
        xor     ecx, ecx                              
        and     ebx, 00000001H                        
        mov     cl,[edx + eax]              
        xor     ebp, ebp                              
        mov     eax, ecx
        mov     cl, bl                                
        shl     cl, 2                                 
        shr     eax, cl                               
        xor     ecx, ecx                              
        mov    [esp + 7CH], edi            
        mov     cx, word ptr [edi]                    
        and     eax, 0000000FH                        
        shl     eax, 16                               
        or      eax, ecx                              
        mov     cx, word ptr [esp + 14H]              
        mov     word ptr [edi], cx                    
        mov     ecx,[esi + 00000C54H]      
        cmp     ebx, ebp                              
        lea     edi, [edx + ecx]                      
        mov     ecx,[esp + 14H]            
        jnz     @265                                 
        mov     dl,[edi]                    
        sar     ecx, 16
        and     dl, 0F0H                              
        or      cl, dl                                
        mov    [edi], cl                    
        jmp     @266

@265:   mov     bl,[edi]                    
        sar     ecx, 12                               
        mov     dl, cl                                
        xor     dl, bl                                
        and     dl, 0FH                               
        xor     dl, cl                                
        mov    [edi], dl                    
@266:   mov     ecx,[esp + 1CH]            
        mov    [esp + 1CH], eax            
        mov     eax,[esi + 34H]
        mov    [esp + 14H], ecx            
        cmp     ecx, eax
        jne     @264                                 
        mov     ecx, eax                              
        mov     al,[esi + 10H]              
        test    al, al                                
        mov    [esi + 38H], ecx            
        mov    [esi + 00000440H], ebp      
        je      @268                                 
        lea     eax, [esi + 00000444H]                
        mov    [esi + 14H], ebp            
        push    eax                                   
        push    ecx                                   
        mov    [esi + 18H], ebp            
        call    @006                                 
        mov     edx,[esi + 38H]            
        mov     edi,[esi + 00000C54H]      
        mov     ecx, edx                              
        xor     ebx, ebx                              
        shr     ecx, 1                                
        mov    [esi + 3CH], eax            
        mov     bl,[ecx + edi]              
        mov     cl, dl                                
        and     cl, 01H                               
        xor     edi, edi
        shl     cl, 2                                 
        shr     ebx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     di, word ptr [ecx + edx*2]            
        mov     edx,[esi + 00000440H]      
        mov     ecx,[esi + 14H]            
        and     ebx, 0000000FH                        
        shl     ebx, 16                               
        or      ebx, edi                              
        inc     edx                                   
        cmp     ecx, ebp                              
        mov    [esi + 38H], ebx            
        mov    [esi + 00000440H], edx      
        jnz     @267                                 
        mov     ecx,[esi + 18H]            
        mov     edx,dword ptr [_BZ2_rNums + ecx*4]
        inc     ecx                                   
        cmp     ecx, 512
        mov    [esi + 14H], edx            
        mov    [esi + 18H], ecx            
        jnz     @267                                 
        mov    [esi + 18H], ebp
@267:   mov     ebx,[esi + 14H]            
        xor     edx, edx                              
        dec     ebx                                   
        mov     ecx, ebx                              
        mov    [esi + 14H], ebx            
        mov     ebx,[esp + 10H]
        cmp     ecx, 1                                
        sete    dl                                    
        xor     edx, eax                              
        xor     ebp, ebp                              
        mov    [esi + 3CH], edx            
        xor     edi, edi                              
        jmp     @305

@268:   lea     eax, [esi + 00000444H]                
        push    eax                                   
        push    ecx                                   
        call    @006
        mov     edx,[esi + 00000C54H]      
        mov    [esi + 3CH], eax            
        mov     eax,[esi + 38H]            
        xor     ebx, ebx                              
        mov     ecx, eax                              
        xor     ebp, ebp                              
        shr     ecx, 1                                
        mov     bl,[ecx + edx]              
        mov     cl, al                                
        and     cl, 01H                               
        xor     edx, edx                              
        shl     cl, 2                                 
        shr     ebx, cl                               
        mov     ecx,[esi + 00000C50H]      
        mov     dx, word ptr [ecx + eax*2]            
        mov     eax,[esi + 00000440H]      
        and     ebx, 0000000FH                        
        shl     ebx, 16
        or      ebx, edx                              
        inc     eax                                   
        mov    [esi + 38H], ebx            
        mov     ebx,[esp + 10H]            
        mov    [esi + 00000440H], eax
        xor     edi, edi                              
        jmp     @305                                 

@269:   xor     edx, edx                              
        cmp     edi, ebp                              
        mov    [esp + 14H], edx            
        jle     @271                                 
@270:   mov     ecx,[esi + 00000C4CH]      
        mov     al,[ecx + edx*4]            
        mov    [esp + 28H], al              
        mov     eax,[esp + 28H]            
        and     eax, 000000FFH                        
        mov     edi,[esi + eax*4 + 00000444H]
        lea     eax, [esi + eax*4 + 00000444H]        
        lea     ecx, [ecx + edi*4]                    
        mov     edi, edx                              
        shl     edi, 8
        or     [ecx], edi
        mov     edi,[eax]                  
        inc     edi                                   
        inc     edx                                   
        mov    [eax], edi                  
        mov     eax,[esp + 18H]            
        cmp     edx, eax                              
        jl      @270                                 
        mov    [esp + 14H], edx            
@271:   mov     eax,[esi + 00000C4CH]      
        mov     ecx,[esi + 34H]            
        mov     edx,[eax + ecx*4]          
        mov     cl,[esi + 10H]
        shr     edx, 8                                
        test    cl, cl                                
        mov    [esi + 38H], edx            
        mov    [esi + 00000440H], ebp      
        jz      @272                                 
        mov     ecx, edx                              
        mov    [esi + 14H], ebp            
        mov    [esi + 18H], ebp            
        mov     edx,[eax + ecx*4]          
        xor     eax, eax                              
        mov     edi, edx                              
        mov    [esi + 38H], edx            
        shr     edi, 8                                
        mov    [esi + 38H], edi            
        mov     al, dl                                
        mov     edi, 1                                
        mov    [esi + 3CH], eax            
        mov    [esi + 00000440H], edi      
        mov     ecx,dword ptr [_BZ2_rNums]
        mov     edx, ecx                              
        mov    [esi + 14H], ecx            
        dec     edx                                   
        mov    [esi + 18H], edi
        mov    [esi + 14H], edx            
        mov     eax, edx                              
        xor     edx, edx                              
        cmp     eax, edi                              
        mov     eax,[esi + 3CH]            
        sete    dl                                    
        xor     eax, edx                              
        xor     edi, edi                              
        mov    [esi + 3CH], eax            
        jmp     @305

@272:   mov     ecx,[esi + 38H]
        mov     edx,[eax + ecx*4]
        xor     eax, eax
        mov     al, dl
        mov    [esi + 38H], edx
        mov    [esi + 3CH], eax
        mov     eax, edx
        shr     eax, 8
        mov    [esi + 38H], eax
        mov     dword ptr [esi + 00000440H], 1
@273:   xor     edi, edi
        jmp     @305

@274:   mov     dword ptr [esi + 04H], 42             
@275:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @276                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @275                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @275

@276:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
        and     eax, 000000FFH                        
        cmp     al, 114                               
        jz      @277                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4
        jmp     @305

@277:   mov     dword ptr [esi + 04H], 43             
@278:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @279
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @278                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @278                                 

@279:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
        and     eax, 000000FFH                        
        cmp     al, 69                                
        jz      @280
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@280:   mov     dword ptr [esi + 04H], 44
@281:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @282                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx
        jnz     @281                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @281                                 

@282:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
        and     eax, 000000FFH                        
        cmp     al, 56                                
        jz      @283                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@283:   mov     dword ptr [esi + 04H], 45             
@284:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @285                                 
        mov     eax,[esi]
        mov     edx,[eax + 04H]
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @284                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @284                                 

@285:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx
        and     eax, 000000FFH                        
        cmp     al, 80                                
        jz      @286                                 
        mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@286:   mov     dword ptr [esi + 04H], 46             
@287:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @288                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @287                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @287

@288:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]
        add     edx, -8                               
        shr     eax, cl                               
        mov    [esi + 20H], edx            
        and     eax, 000000FFH                        
        cmp     al, -112                              
        jz      @290                                 
@289:   mov     ebx,[esp + 10H]            
        mov     edi, -4                               
        jmp     @305

@290:   mov     dword ptr [esi + 00000C5CH], 0

@291:   mov     dword ptr [esi + 04H], 47
@292:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @293                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @292                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @292                                 

@293:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C5CH]      
        mov    [esi + 20H], edx
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax
        mov    [esi + 00000C5CH], ecx      

@294:
        mov     dword ptr [esi + 04H], 48             
@295:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @296                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @295                                 
        inc     dword ptr [eax + 0CH]
        jmp     @295                                 

@296:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C5CH]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C5CH], ecx      

@297:   mov     dword ptr [esi + 04H], 49
@298:   mov     ecx,[esi + 20H]
        cmp     ecx, 8                                
        jge     @299                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx
        je      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]            
        xor     ebx, ebx                              
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx                              
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx
        mov    [eax], ebx
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi                                   
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]                  
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @298                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @298                                 

@299:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        lea     ecx, [edx - 08H]
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C5CH]      
        mov    [esi + 20H], edx            
        shl     ecx, 8
        and     eax, 000000FFH                        
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C5CH], ecx      

@300:
        mov     dword ptr [esi + 04H], 50             
@301:   mov     ecx,[esi + 20H]            
        cmp     ecx, 8                                
        jge     @302                                 
        mov     eax,[esi]                  
        mov     edx,[eax + 04H]            
        test    edx, edx                              
        jz      @303                                 
        mov     edi,[eax]                  
        mov     edx,[esi + 1CH]
        xor     ebx, ebx
        add     ecx, 8                                
        mov     bl,[edi]                    
        mov    [esi + 20H], ecx            
        shl     edx, 8                                
        or      edx, ebx
        mov    [esi + 1CH], edx            
        mov     ebx,[eax]                  
        inc     ebx                                   
        mov    [eax], ebx                  
        mov     eax,[esi]                  
        mov     edi,[eax + 04H]            
        dec     edi
        mov    [eax + 04H], edi            
        mov     eax,[esi]                  
        mov     edx,[eax + 08H]            
        inc     edx                                   
        mov    [eax + 08H], edx            
        mov     eax,[esi]
        mov     ecx,[eax + 08H]            
        test    ecx, ecx                              
        jnz     @301                                 
        inc     dword ptr [eax + 0CH]                 
        jmp     @301                                 

@302:   mov     edx,[esi + 20H]            
        mov     eax,[esi + 1CH]            
        mov     ebx,[esp + 10H]            
        mov     dword ptr [esi + 04H], 1              
        lea     ecx, [edx - 08H]                      
        add     edx, -8                               
        shr     eax, cl                               
        mov     ecx,[esi + 00000C5CH]      
        mov    [esi + 20H], edx            
        shl     ecx, 8                                
        mov     edi, 4                                
        and     eax, 000000FFH
        and     eax, 000000FFH                        
        or      ecx, eax                              
        mov    [esi + 00000C5CH], ecx      
        jmp     @305

@303:   mov     ebx,[esp + 10H]
        xor     edi, edi

@305:   mov     edx,[esp + 14H]
        mov     eax,[esp + 1CH]
        mov     ecx,[esp + 2CH]
        mov    [esi + 0000FA14H], edx      
        mov     edx,[esp + 4CH]            
        mov    [esi + 0000FA18H], eax      
        mov     eax,[esp + 5CH]            
        mov    [esi + 0000FA1CH], ecx      
        mov     ecx,[esp + 34H]            
        mov    [esi + 0000FA20H], edx      
        mov     edx,[esp + 6CH]            
        mov    [esi + 0000FA24H], eax      
        mov     eax,[esp + 50H]            
        mov    [esi + 0000FA28H], ecx      
        mov     ecx,[esp + 38H]            
        mov    [esi + 0000FA2CH], edx
        mov     edx,[esp + 3CH]            
        mov    [esi + 0000FA30H], eax      
        mov     eax,[esp + 60H]
        mov    [esi + 0000FA34H], ecx      
        mov     ecx,[esp + 18H]            
        mov    [esi + 0000FA38H], edx      
        mov     edx,[esp + 20H]            
        mov    [esi + 0000FA3CH], eax      
        mov     eax,[esp + 54H]            
        mov    [esi + 0000FA40H], ecx      
        mov     ecx,[esp + 30H]            
        mov    [esi + 0000FA44H], edx      
        mov     edx,[esp + 00000084H]      
        mov    [esi + 0000FA48H], eax      
        mov     eax,[esp + 24H]            
        mov    [esi + 0000FA4CH], ecx
        mov     ecx,[esp + 64H]            
        mov    [esi + 0000FA58H], eax      
        mov     eax,[esp + 58H]            
        mov    [esi + 0000FA50H], edx      
        mov     edx,[esp + 68H]            
        mov    [esi + 0000FA64H], eax      
        mov     eax,[esp + 48H]            
        mov    [esi + 0000FA5CH], ecx      
        mov     ecx,[esp + 40H]            
        mov    [esi + 0000FA60H], edx      
        mov     edx,[esp + 44H]            
        mov    [esi + 0000FA70H], eax      
        mov     eax, edi
        mov    [esi + 0000FA54H], ebx      
        mov    [esi + 0000FA68H], ecx      
        mov    [esi + 0000FA6CH], edx      
        pop     edi                                   
        pop     esi
        pop     ebp
        pop     ebx
        add     esp, 120
        ret 4

@304:   push    4001
        call    BzExcept
        push    4002
        call    BzExcept

//        nop; nop // align Table
@306:   dd @072
        dd @076
        dd @079
        dd @082                               
        dd @088                               
        dd @091                               
        dd @094                               
        dd @097                               
        dd @100                               
        dd @103                               
        dd @107                               
        dd @110                               
        dd @113
        dd @116                               
        dd @119                               
        dd @122
        dd @125                               
        dd @128                               
        dd @133                               
        dd @139                               
        dd @148                               
        dd @152                               
        dd @156                               
        dd @179                               
        dd @174                               
        dd @168                               
        dd @192                               
        dd @195
        dd @238
        dd @244
        dd @229
        dd @203
        dd @274
        dd @277
        dd @280
        dd @283
        dd @286
        dd @291
        dd @294
        dd @297
        dd @300
@307:
        mov     eax,[esp + 04H]
        xor     ecx, ecx
        mov     dword ptr [eax + 00000C68H], 0
@308:   mov     dl,[eax + ecx + 00000C6CH]
        test    dl, dl
        jz      @309
        mov     edx,[eax + 00000C68H]
        mov    [eax + edx + 00000D7CH], cl
        mov     edx,[eax + 00000C68H]
        inc     edx
        mov    [eax + 00000C68H], edx
@309:   inc     ecx
        cmp     ecx, 256
        jl      @308
        ret     4
@310:
        mov     eax,[esp + 14H]
        push    ebx
        mov     ebx,[esp + 20H]
        push    ebp
        mov     ebp,[esp + 20H]
        push    esi
        xor     esi, esi
        push    edi
        cmp     eax, ebp
        mov     edi, eax
        jg      @315
@311:   xor     eax, eax
        test    ebx, ebx
        jle     @314
        mov     ecx,[esp + 1CH]
        lea     ecx, [ecx + esi*4]
@312:   mov     ebx,[esp + 20H]
        xor     edx, edx
        mov     dl,[eax + ebx]
        cmp     edx, edi
        jnz     @313                                 
        mov    [ecx], eax                  
        inc     esi                                   
        add     ecx, 4
@313:   mov     ebx,[esp + 2CH]            
        inc     eax
        cmp     eax, ebx                              
        jl      @312                                 
@314:   inc     edi
        cmp     edi, ebp                              
        jle     @311                                 
@315:   mov     edx,[esp + 18H]            
        mov     ecx, 23                               
        xor     eax, eax                              
        mov     edi, edx                              
        test    ebx, ebx                              
        rep stosd                                     
        jle     @317                                 
@316:   mov     esi,[esp + 20H]            
        xor     ecx, ecx                              
        mov     cl,[eax + esi]              
        mov     edi,[edx + ecx*4 + 04H]    
        lea     ecx, [edx + ecx*4 + 04H]              
        inc     edi                                   
        inc     eax                                   
        cmp     eax, ebx                              
        mov    [ecx], edi
        jl      @316
@317:   lea     eax, [edx + 04H]                      
        mov     ecx, 22                               
@318:   mov     esi,[eax - 04H]            
        mov     ebx,[eax]
        add     ebx, esi                              
        mov    [eax], ebx                  
        add     eax, 4                                
        dec     ecx                                   
        jnz     @318                                 
        mov     ebx,[esp + 14H]            
        mov     ecx, 23                               
        xor     eax, eax
        mov     edi, ebx
        rep stosd                                     
        mov     ecx,[esp + 24H]            
        xor     esi, esi                              
        cmp     ecx, ebp                              
        jg      @320                                 
        lea     eax, [edx + ecx*4]                    
        mov     edi, ebx                              
        mov     ebx,[esp + 24H]            
        mov     ecx, ebp                              
        sub     ecx, ebx                              
        sub     edi, edx                              
        inc     ecx                                   
@319:   mov     ebx,[eax + 04H]
        sub     ebx,[eax]
        add     eax, 4
        add     esi, ebx
        lea     ebx, [esi - 01H]
        mov    [edi + eax - 04H], ebx
        shl     esi, 1
        dec     ecx
        jnz     @319
        mov     ebx,[esp + 14H]
        mov     ecx,[esp + 24H]
@320:   inc     ecx
        cmp     ecx, ebp
        jg      @322
        sub     ebp, ecx
        lea     eax, [edx + ecx*4]
        lea     edx, [ebx + ecx*4 - 04H]
        inc     ebp
@321:   mov     ecx,[edx]
        mov     ebx,[eax]
        add     edx, 4
        add     eax, 4
        lea     ecx, [ecx + ecx + 02H]
        sub     ecx, ebx
        dec     ebp
        mov    [eax - 04H], ecx
        jnz     @321
@322:   pop     edi
        pop     esi
        pop     ebp
        pop     ebx
        ret     28
end;

function BZ2_bzDecompressEnd(var strm: TBZStreamRec): Integer; cdecl;
asm pop ebp  // auto-generated push ebp; mov ebp,esp
        push    esi
        mov     esi,[esp + 08H]
        test    esi, esi
        push    edi
        jnz     @064
        pop     edi
        mov     eax, -2
        pop     esi                                   
        ret                                          

@064:   mov     edi,[esi + 20H]            
        test    edi, edi                              
        jnz     @065                                 
        pop     edi                                   
        mov     eax, -2                               
        pop     esi                                   
        ret                                          

@065:   cmp    [edi], esi                  
        jz      @066                                 
        pop     edi                                   
        mov     eax, -2                               
        pop     esi
        ret                                          

@066:   mov     eax,[edi + 00000C4CH]      
        test    eax, eax                              
        jz      @067                                 
        push    eax                                   
        mov     eax,[esi + 2CH]            
        push    eax                                   
        call    dword ptr [esi + 28H]                 
@067:   mov     eax,[edi + 00000C50H]      
        test    eax, eax                              
        jz      @068                                 
        mov     ecx,[esi + 2CH]
        push    eax                                   
        push    ecx                                   
        call    dword ptr [esi + 28H]                 
@068:   mov     edi,[edi + 00000C54H]      
        test    edi, edi                              
        jz      @069                                 
        mov     edx,[esi + 2CH]
        push    edi                                   
        push    edx                                   
        call    dword ptr [esi + 28H]                 
@069:   mov     eax,[esi + 20H]
        mov     ecx,[esi + 2CH]
        push    eax                                   
        push    ecx                                   
        call    dword ptr [esi + 28H]
        mov     dword ptr [esi + 20H], 0              
        pop     edi
        xor     eax, eax
        pop     esi
        ret
end;


