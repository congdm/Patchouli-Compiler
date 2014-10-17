Microsoft (R) COFF/PE Dumper Version 10.00.30319.01
Copyright (C) Microsoft Corporation.  All rights reserved.


Dump of file Console.dll

File Type: DLL

  0000000010002000: 55                 push        rbp
  0000000010002001: 48 8B EC           mov         rbp,rsp
  0000000010002004: 48 83 EC 10        sub         rsp,10h
  0000000010002008: 48 89 4D 10        mov         qword ptr [rbp+10h],rcx
  000000001000200C: 48 83 EC 30        sub         rsp,30h
  0000000010002010: 48 8B 0D 41 FF FF  mov         rcx,qword ptr [10001F58h]
                    FF
  0000000010002017: 48 8B 09           mov         rcx,qword ptr [rcx]
  000000001000201A: 51                 push        rcx
  000000001000201B: 48 8B 0D CE FF FF  mov         rcx,qword ptr [10001FF0h]
                    FF
  0000000010002022: 51                 push        rcx
  0000000010002023: 48 83 EC 20        sub         rsp,20h
  0000000010002027: 48 8D 4D 10        lea         rcx,[rbp+10h]
  000000001000202B: BA 02 00 00 00     mov         edx,2
  0000000010002030: FF 15 1A FF FF FF  call        qword ptr [10001F50h]
  0000000010002036: 48 83 C4 20        add         rsp,20h
  000000001000203A: 59                 pop         rcx
  000000001000203B: 48 8B D0           mov         rdx,rax
  000000001000203E: 41 B8 01 00 00 00  mov         r8d,1
  0000000010002044: 51                 push        rcx
  0000000010002045: 52                 push        rdx
  0000000010002046: 41 50              push        r8
  0000000010002048: 48 83 EC 20        sub         rsp,20h
  000000001000204C: 48 8D 4D FC        lea         rcx,[rbp-4]
  0000000010002050: BA 04 00 00 00     mov         edx,4
  0000000010002055: FF 15 F5 FE FF FF  call        qword ptr [10001F50h]
  000000001000205B: 48 83 C4 20        add         rsp,20h
  000000001000205F: 41 58              pop         r8
  0000000010002061: 5A                 pop         rdx
  0000000010002062: 59                 pop         rcx
  0000000010002063: 4C 8B C8           mov         r9,rax
  0000000010002066: 48 8B 05 7B FF FF  mov         rax,qword ptr [10001FE8h]
                    FF
  000000001000206D: 48 89 44 24 28     mov         qword ptr [rsp+28h],rax
  0000000010002072: 58                 pop         rax
  0000000010002073: FF D0              call        rax
  0000000010002075: 48 83 C4 30        add         rsp,30h
  0000000010002079: 89 45 F8           mov         dword ptr [rbp-8],eax
  000000001000207C: 48 83 EC 20        sub         rsp,20h
  0000000010002080: 8B 4D F8           mov         ecx,dword ptr [rbp-8]
  0000000010002083: FF 15 BF FE FF FF  call        qword ptr [10001F48h]
  0000000010002089: 48 83 C4 20        add         rsp,20h
  000000001000208D: 8B 4D FC           mov         ecx,dword ptr [rbp-4]
  0000000010002090: 83 F9 01           cmp         ecx,1
  0000000010002093: C9                 leave
  0000000010002094: C3                 ret
  0000000010002095: 55                 push        rbp
  0000000010002096: 48 8B EC           mov         rbp,rsp
  0000000010002099: 48 83 EC 10        sub         rsp,10h
  000000001000209D: B8 0D 00 00 00     mov         eax,0Dh
  00000000100020A2: 66 89 45 FC        mov         word ptr [rbp-4],ax
  00000000100020A6: B8 0A 00 00 00     mov         eax,0Ah
  00000000100020AB: 66 89 45 FE        mov         word ptr [rbp-2],ax
  00000000100020AF: 48 83 EC 30        sub         rsp,30h
  00000000100020B3: 48 8B 0D 9E FE FF  mov         rcx,qword ptr [10001F58h]
                    FF
  00000000100020BA: 48 8B 09           mov         rcx,qword ptr [rcx]
  00000000100020BD: 51                 push        rcx
  00000000100020BE: 48 8B 0D 2B FF FF  mov         rcx,qword ptr [10001FF0h]
                    FF
  00000000100020C5: 51                 push        rcx
  00000000100020C6: 48 83 EC 20        sub         rsp,20h
  00000000100020CA: 48 8D 4D FC        lea         rcx,[rbp-4]
  00000000100020CE: BA 04 00 00 00     mov         edx,4
  00000000100020D3: FF 15 77 FE FF FF  call        qword ptr [10001F50h]
  00000000100020D9: 48 83 C4 20        add         rsp,20h
  00000000100020DD: 59                 pop         rcx
  00000000100020DE: 48 8B D0           mov         rdx,rax
  00000000100020E1: 41 B8 02 00 00 00  mov         r8d,2
  00000000100020E7: 51                 push        rcx
  00000000100020E8: 52                 push        rdx
  00000000100020E9: 41 50              push        r8
  00000000100020EB: 48 83 EC 20        sub         rsp,20h
  00000000100020EF: 48 8D 4D F8        lea         rcx,[rbp-8]
  00000000100020F3: BA 04 00 00 00     mov         edx,4
  00000000100020F8: FF 15 52 FE FF FF  call        qword ptr [10001F50h]
  00000000100020FE: 48 83 C4 20        add         rsp,20h
  0000000010002102: 41 58              pop         r8
  0000000010002104: 5A                 pop         rdx
  0000000010002105: 59                 pop         rcx
  0000000010002106: 4C 8B C8           mov         r9,rax
  0000000010002109: 48 8B 05 D8 FE FF  mov         rax,qword ptr [10001FE8h]
                    FF
  0000000010002110: 48 89 44 24 28     mov         qword ptr [rsp+28h],rax
  0000000010002115: 58                 pop         rax
  0000000010002116: FF D0              call        rax
  0000000010002118: 48 83 C4 30        add         rsp,30h
  000000001000211C: 89 45 F4           mov         dword ptr [rbp-0Ch],eax
  000000001000211F: 48 83 EC 20        sub         rsp,20h
  0000000010002123: 8B 4D F4           mov         ecx,dword ptr [rbp-0Ch]
  0000000010002126: FF 15 1C FE FF FF  call        qword ptr [10001F48h]
  000000001000212C: 48 83 C4 20        add         rsp,20h
  0000000010002130: 8B 4D F8           mov         ecx,dword ptr [rbp-8]
  0000000010002133: 83 F9 02           cmp         ecx,2
  0000000010002136: C9                 leave
  0000000010002137: C3                 ret
  0000000010002138: 55                 push        rbp
  0000000010002139: 48 8B EC           mov         rbp,rsp
  000000001000213C: 48 83 EC 10        sub         rsp,10h
  0000000010002140: 48 89 4D 10        mov         qword ptr [rbp+10h],rcx
  0000000010002144: 48 89 55 18        mov         qword ptr [rbp+18h],rdx
  0000000010002148: 48 83 EC 30        sub         rsp,30h
  000000001000214C: 48 8B 0D 05 FE FF  mov         rcx,qword ptr [10001F58h]
                    FF
  0000000010002153: 48 8B 09           mov         rcx,qword ptr [rcx]
  0000000010002156: 51                 push        rcx
  0000000010002157: 48 8B 0D 92 FE FF  mov         rcx,qword ptr [10001FF0h]
                    FF
  000000001000215E: 51                 push        rcx
  000000001000215F: 48 83 EC 20        sub         rsp,20h
  0000000010002163: 48 8B 4D 10        mov         rcx,qword ptr [rbp+10h]
  0000000010002167: BA 10 00 00 00     mov         edx,10h
  000000001000216C: FF 15 DE FD FF FF  call        qword ptr [10001F50h]
  0000000010002172: 48 83 C4 20        add         rsp,20h
  0000000010002176: 59                 pop         rcx
  0000000010002177: 48 8B D0           mov         rdx,rax
  000000001000217A: 51                 push        rcx
  000000001000217B: 52                 push        rdx
  000000001000217C: 48 83 EC 28        sub         rsp,28h
  0000000010002180: 48 8B 4D 10        mov         rcx,qword ptr [rbp+10h]
  0000000010002184: 48 8B 55 18        mov         rdx,qword ptr [rbp+18h]
  0000000010002188: FF 15 B2 FD FF FF  call        qword ptr [10001F40h]
  000000001000218E: 48 83 C4 28        add         rsp,28h
  0000000010002192: 5A                 pop         rdx
  0000000010002193: 59                 pop         rcx
  0000000010002194: 4C 8B C0           mov         r8,rax
  0000000010002197: 51                 push        rcx
  0000000010002198: 52                 push        rdx
  0000000010002199: 41 50              push        r8
  000000001000219B: 48 83 EC 28        sub         rsp,28h
  000000001000219F: 48 8D 4D FC        lea         rcx,[rbp-4]
  00000000100021A3: BA 04 00 00 00     mov         edx,4
  00000000100021A8: FF 15 A2 FD FF FF  call        qword ptr [10001F50h]
  00000000100021AE: 48 83 C4 28        add         rsp,28h
  00000000100021B2: 41 58              pop         r8
  00000000100021B4: 5A                 pop         rdx
  00000000100021B5: 59                 pop         rcx
  00000000100021B6: 4C 8B C8           mov         r9,rax
  00000000100021B9: 48 8B 05 28 FE FF  mov         rax,qword ptr [10001FE8h]
                    FF
  00000000100021C0: 48 89 44 24 28     mov         qword ptr [rsp+28h],rax
  00000000100021C5: 58                 pop         rax
  00000000100021C6: FF D0              call        rax
  00000000100021C8: 48 83 C4 30        add         rsp,30h
  00000000100021CC: 89 45 F8           mov         dword ptr [rbp-8],eax
  00000000100021CF: C9                 leave
  00000000100021D0: C3                 ret
  00000000100021D1: 55                 push        rbp
  00000000100021D2: 48 8B EC           mov         rbp,rsp
  00000000100021D5: 48 83 EC 60        sub         rsp,60h
  00000000100021D9: 48 89 4D 10        mov         qword ptr [rbp+10h],rcx
  00000000100021DD: 48 89 55 18        mov         qword ptr [rbp+18h],rdx
  00000000100021E1: 4C 89 45 20        mov         qword ptr [rbp+20h],r8
  00000000100021E5: 48 8B 45 10        mov         rax,qword ptr [rbp+10h]
  00000000100021E9: 48 B9 AA AA AA AA  mov         rcx,8AAAAAAAAAAAAAAAh
                    AA AA AA 8A
  00000000100021F3: 48 3B C1           cmp         rax,rcx
  00000000100021F6: 0F 84 5B 03 00 00  je          0000000010002557
  00000000100021FC: 48 8B 45 10        mov         rax,qword ptr [rbp+10h]
  0000000010002200: 48 83 F8 00        cmp         rax,0
  0000000010002204: 0F 8D 2B 00 00 00  jge         0000000010002235
  000000001000220A: B8 01 00 00 00     mov         eax,1
  000000001000220F: 88 45 FF           mov         byte ptr [rbp-1],al
  0000000010002212: 48 8B 45 10        mov         rax,qword ptr [rbp+10h]
  0000000010002216: 48 F7 D8           neg         rax
  0000000010002219: 0F 80 05 00 00 00  jo          0000000010002224
  000000001000221F: E9 08 00 00 00     jmp         000000001000222C
  0000000010002224: B0 00              mov         al,0
  0000000010002226: B9 4B 04 00 00     mov         ecx,44Bh
  000000001000222B: CC                 int         3
  000000001000222C: 48 89 45 10        mov         qword ptr [rbp+10h],rax
  0000000010002230: E9 05 00 00 00     jmp         000000001000223A
  0000000010002235: 31 C0              xor         eax,eax
  0000000010002237: 88 45 FF           mov         byte ptr [rbp-1],al
  000000001000223A: 31 C0              xor         eax,eax
  000000001000223C: 48 89 45 B0        mov         qword ptr [rbp-50h],rax
  0000000010002240: 48 8B 45 B0        mov         rax,qword ptr [rbp-50h]
  0000000010002244: 48 83 F8 20        cmp         rax,20h
  0000000010002248: 0F 83 05 00 00 00  jae         0000000010002253
  000000001000224E: E9 08 00 00 00     jmp         000000001000225B
  0000000010002253: B0 01              mov         al,1
  0000000010002255: B9 87 04 00 00     mov         ecx,487h
  000000001000225A: CC                 int         3
  000000001000225B: 48 C1 E0 01        shl         rax,1
  000000001000225F: 48 8D 4D BE        lea         rcx,[rbp-42h]
  0000000010002263: 48 03 C1           add         rax,rcx
  0000000010002266: 48 8B 4D 10        mov         rcx,qword ptr [rbp+10h]
  000000001000226A: BA 0A 00 00 00     mov         edx,0Ah
  000000001000226F: 48 85 D2           test        rdx,rdx
  0000000010002272: 0F 8E 05 00 00 00  jle         000000001000227D
  0000000010002278: E9 08 00 00 00     jmp         0000000010002285
  000000001000227D: B0 00              mov         al,0
  000000001000227F: B9 99 04 00 00     mov         ecx,499h
  0000000010002284: CC                 int         3
  0000000010002285: 4C 8B C0           mov         r8,rax
  0000000010002288: 4C 8B CA           mov         r9,rdx
  000000001000228B: 48 8B C1           mov         rax,rcx
  000000001000228E: 48 99              cqo
  0000000010002290: 48 85 C0           test        rax,rax
  0000000010002293: 0F 88 08 00 00 00  js          00000000100022A1
  0000000010002299: 49 F7 F9           idiv        rax,r9
  000000001000229C: E9 0F 00 00 00     jmp         00000000100022B0
  00000000100022A1: 49 F7 F9           idiv        rax,r9
  00000000100022A4: 48 85 D2           test        rdx,rdx
  00000000100022A7: 0F 84 03 00 00 00  je          00000000100022B0
  00000000100022AD: 49 03 D1           add         rdx,r9
  00000000100022B0: 48 8B CA           mov         rcx,rdx
  00000000100022B3: 49 8B C0           mov         rax,r8
  00000000100022B6: 48 83 C1 30        add         rcx,30h
  00000000100022BA: 0F 80 05 00 00 00  jo          00000000100022C5
  00000000100022C0: E9 08 00 00 00     jmp         00000000100022CD
  00000000100022C5: B0 00              mov         al,0
  00000000100022C7: B9 A3 04 00 00     mov         ecx,4A3h
  00000000100022CC: CC                 int         3
  00000000100022CD: 66 89 08           mov         word ptr [rax],cx
  00000000100022D0: 48 83 45 B0 01     add         qword ptr [rbp-50h],1
  00000000100022D5: 48 8B 45 10        mov         rax,qword ptr [rbp+10h]
  00000000100022D9: B9 0A 00 00 00     mov         ecx,0Ah
  00000000100022DE: 48 85 C9           test        rcx,rcx
  00000000100022E1: 0F 8E 05 00 00 00  jle         00000000100022EC
  00000000100022E7: E9 08 00 00 00     jmp         00000000100022F4
  00000000100022EC: B0 00              mov         al,0
  00000000100022EE: B9 C8 04 00 00     mov         ecx,4C8h
  00000000100022F3: CC                 int         3
  00000000100022F4: 48 99              cqo
  00000000100022F6: 48 85 C0           test        rax,rax
  00000000100022F9: 0F 88 08 00 00 00  js          0000000010002307
  00000000100022FF: 48 F7 F9           idiv        rax,rcx
  0000000010002302: E9 10 00 00 00     jmp         0000000010002317
  0000000010002307: 48 F7 F9           idiv        rax,rcx
  000000001000230A: 48 85 D2           test        rdx,rdx
  000000001000230D: 0F 84 04 00 00 00  je          0000000010002317
  0000000010002313: 48 83 E8 01        sub         rax,1
  0000000010002317: 48 89 45 10        mov         qword ptr [rbp+10h],rax
  000000001000231B: 48 8B 45 10        mov         rax,qword ptr [rbp+10h]
  000000001000231F: 48 83 F8 00        cmp         rax,0
  0000000010002323: 0F 85 17 FF FF FF  jne         0000000010002240
  0000000010002329: 0F B6 45 FF        movzx       eax,byte ptr [rbp-1]
  000000001000232D: 85 C0              test        eax,eax
  000000001000232F: 0F 84 2E 01 00 00  je          0000000010002463
  0000000010002335: B8 2D 00 00 00     mov         eax,2Dh
  000000001000233A: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  000000001000233E: 66 89 01           mov         word ptr [rcx],ax
  0000000010002341: 31 C0              xor         eax,eax
  0000000010002343: 48 89 45 A8        mov         qword ptr [rbp-58h],rax
  0000000010002347: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  000000001000234B: 48 8B 4D B0        mov         rcx,qword ptr [rbp-50h]
  000000001000234F: 48 3B C1           cmp         rax,rcx
  0000000010002352: 0F 8D C1 00 00 00  jge         0000000010002419
  0000000010002358: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  000000001000235C: 48 83 C0 01        add         rax,1
  0000000010002360: 0F 80 05 00 00 00  jo          000000001000236B
  0000000010002366: E9 08 00 00 00     jmp         0000000010002373
  000000001000236B: B0 00              mov         al,0
  000000001000236D: B9 1C 05 00 00     mov         ecx,51Ch
  0000000010002372: CC                 int         3
  0000000010002373: 48 8B 4D 20        mov         rcx,qword ptr [rbp+20h]
  0000000010002377: 48 3B C8           cmp         rcx,rax
  000000001000237A: 0F 86 05 00 00 00  jbe         0000000010002385
  0000000010002380: E9 08 00 00 00     jmp         000000001000238D
  0000000010002385: B0 01              mov         al,1
  0000000010002387: B9 1C 05 00 00     mov         ecx,51Ch
  000000001000238C: CC                 int         3
  000000001000238D: 48 C1 E0 01        shl         rax,1
  0000000010002391: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  0000000010002395: 48 03 C1           add         rax,rcx
  0000000010002398: 48 8B 4D B0        mov         rcx,qword ptr [rbp-50h]
  000000001000239C: 48 83 E9 01        sub         rcx,1
  00000000100023A0: 0F 80 05 00 00 00  jo          00000000100023AB
  00000000100023A6: E9 08 00 00 00     jmp         00000000100023B3
  00000000100023AB: B0 00              mov         al,0
  00000000100023AD: B9 29 05 00 00     mov         ecx,529h
  00000000100023B2: CC                 int         3
  00000000100023B3: 48 8B 55 A8        mov         rdx,qword ptr [rbp-58h]
  00000000100023B7: 48 2B CA           sub         rcx,rdx
  00000000100023BA: 0F 80 05 00 00 00  jo          00000000100023C5
  00000000100023C0: E9 08 00 00 00     jmp         00000000100023CD
  00000000100023C5: B0 00              mov         al,0
  00000000100023C7: B9 2C 05 00 00     mov         ecx,52Ch
  00000000100023CC: CC                 int         3
  00000000100023CD: 48 83 F9 20        cmp         rcx,20h
  00000000100023D1: 0F 83 05 00 00 00  jae         00000000100023DC
  00000000100023D7: E9 08 00 00 00     jmp         00000000100023E4
  00000000100023DC: B0 01              mov         al,1
  00000000100023DE: B9 2C 05 00 00     mov         ecx,52Ch
  00000000100023E3: CC                 int         3
  00000000100023E4: 48 C1 E1 01        shl         rcx,1
  00000000100023E8: 48 8D 55 BE        lea         rdx,[rbp-42h]
  00000000100023EC: 48 03 CA           add         rcx,rdx
  00000000100023EF: 0F B6 09           movzx       ecx,byte ptr [rcx]
  00000000100023F2: 66 89 08           mov         word ptr [rax],cx
  00000000100023F5: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  00000000100023F9: 48 83 C0 01        add         rax,1
  00000000100023FD: 0F 80 05 00 00 00  jo          0000000010002408
  0000000010002403: E9 08 00 00 00     jmp         0000000010002410
  0000000010002408: B0 00              mov         al,0
  000000001000240A: B9 3C 05 00 00     mov         ecx,53Ch
  000000001000240F: CC                 int         3
  0000000010002410: 48 89 45 A8        mov         qword ptr [rbp-58h],rax
  0000000010002414: E9 2E FF FF FF     jmp         0000000010002347
  0000000010002419: 48 8B 45 B0        mov         rax,qword ptr [rbp-50h]
  000000001000241D: 48 83 C0 01        add         rax,1
  0000000010002421: 0F 80 05 00 00 00  jo          000000001000242C
  0000000010002427: E9 08 00 00 00     jmp         0000000010002434
  000000001000242C: B0 00              mov         al,0
  000000001000242E: B9 4C 05 00 00     mov         ecx,54Ch
  0000000010002433: CC                 int         3
  0000000010002434: 48 8B 4D 20        mov         rcx,qword ptr [rbp+20h]
  0000000010002438: 48 3B C8           cmp         rcx,rax
  000000001000243B: 0F 86 05 00 00 00  jbe         0000000010002446
  0000000010002441: E9 08 00 00 00     jmp         000000001000244E
  0000000010002446: B0 01              mov         al,1
  0000000010002448: B9 4C 05 00 00     mov         ecx,54Ch
  000000001000244D: CC                 int         3
  000000001000244E: 48 C1 E0 01        shl         rax,1
  0000000010002452: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  0000000010002456: 48 03 C1           add         rax,rcx
  0000000010002459: 31 C9              xor         ecx,ecx
  000000001000245B: 66 89 08           mov         word ptr [rax],cx
  000000001000245E: E9 EF 00 00 00     jmp         0000000010002552
  0000000010002463: 31 C0              xor         eax,eax
  0000000010002465: 48 89 45 A8        mov         qword ptr [rbp-58h],rax
  0000000010002469: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  000000001000246D: 48 8B 4D B0        mov         rcx,qword ptr [rbp-50h]
  0000000010002471: 48 3B C1           cmp         rax,rcx
  0000000010002474: 0F 8D AA 00 00 00  jge         0000000010002524
  000000001000247A: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  000000001000247E: 48 8B 4D 20        mov         rcx,qword ptr [rbp+20h]
  0000000010002482: 48 3B C8           cmp         rcx,rax
  0000000010002485: 0F 86 05 00 00 00  jbe         0000000010002490
  000000001000248B: E9 08 00 00 00     jmp         0000000010002498
  0000000010002490: B0 01              mov         al,1
  0000000010002492: B9 7C 05 00 00     mov         ecx,57Ch
  0000000010002497: CC                 int         3
  0000000010002498: 48 C1 E0 01        shl         rax,1
  000000001000249C: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  00000000100024A0: 48 03 C1           add         rax,rcx
  00000000100024A3: 48 8B 4D B0        mov         rcx,qword ptr [rbp-50h]
  00000000100024A7: 48 83 E9 01        sub         rcx,1
  00000000100024AB: 0F 80 05 00 00 00  jo          00000000100024B6
  00000000100024B1: E9 08 00 00 00     jmp         00000000100024BE
  00000000100024B6: B0 00              mov         al,0
  00000000100024B8: B9 89 05 00 00     mov         ecx,589h
  00000000100024BD: CC                 int         3
  00000000100024BE: 48 8B 55 A8        mov         rdx,qword ptr [rbp-58h]
  00000000100024C2: 48 2B CA           sub         rcx,rdx
  00000000100024C5: 0F 80 05 00 00 00  jo          00000000100024D0
  00000000100024CB: E9 08 00 00 00     jmp         00000000100024D8
  00000000100024D0: B0 00              mov         al,0
  00000000100024D2: B9 8C 05 00 00     mov         ecx,58Ch
  00000000100024D7: CC                 int         3
  00000000100024D8: 48 83 F9 20        cmp         rcx,20h
  00000000100024DC: 0F 83 05 00 00 00  jae         00000000100024E7
  00000000100024E2: E9 08 00 00 00     jmp         00000000100024EF
  00000000100024E7: B0 01              mov         al,1
  00000000100024E9: B9 8C 05 00 00     mov         ecx,58Ch
  00000000100024EE: CC                 int         3
  00000000100024EF: 48 C1 E1 01        shl         rcx,1
  00000000100024F3: 48 8D 55 BE        lea         rdx,[rbp-42h]
  00000000100024F7: 48 03 CA           add         rcx,rdx
  00000000100024FA: 0F B6 09           movzx       ecx,byte ptr [rcx]
  00000000100024FD: 66 89 08           mov         word ptr [rax],cx
  0000000010002500: 48 8B 45 A8        mov         rax,qword ptr [rbp-58h]
  0000000010002504: 48 83 C0 01        add         rax,1
  0000000010002508: 0F 80 05 00 00 00  jo          0000000010002513
  000000001000250E: E9 08 00 00 00     jmp         000000001000251B
  0000000010002513: B0 00              mov         al,0
  0000000010002515: B9 9C 05 00 00     mov         ecx,59Ch
  000000001000251A: CC                 int         3
  000000001000251B: 48 89 45 A8        mov         qword ptr [rbp-58h],rax
  000000001000251F: E9 45 FF FF FF     jmp         0000000010002469
  0000000010002524: 48 8B 45 B0        mov         rax,qword ptr [rbp-50h]
  0000000010002528: 48 8B 4D 20        mov         rcx,qword ptr [rbp+20h]
  000000001000252C: 48 3B C8           cmp         rcx,rax
  000000001000252F: 0F 86 05 00 00 00  jbe         000000001000253A
  0000000010002535: E9 08 00 00 00     jmp         0000000010002542
  000000001000253A: B0 01              mov         al,1
  000000001000253C: B9 A9 05 00 00     mov         ecx,5A9h
  0000000010002541: CC                 int         3
  0000000010002542: 48 C1 E0 01        shl         rax,1
  0000000010002546: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  000000001000254A: 48 03 C1           add         rax,rcx
  000000001000254D: 31 C9              xor         ecx,ecx
  000000001000254F: 66 89 08           mov         word ptr [rax],cx
  0000000010002552: E9 2B 00 00 00     jmp         0000000010002582
  0000000010002557: 31 C0              xor         eax,eax
  0000000010002559: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  000000001000255D: 66 89 01           mov         word ptr [rcx],ax
  0000000010002560: 48 83 EC 20        sub         rsp,20h
  0000000010002564: 48 8D 0D 93 F9 FF  lea         rcx,[10001EFEh]
                    FF
  000000001000256B: BA 15 00 00 00     mov         edx,15h
  0000000010002570: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002574: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002578: FF 15 AA F9 FF FF  call        qword ptr [10001F28h]
  000000001000257E: 48 83 C4 20        add         rsp,20h
  0000000010002582: C9                 leave
  0000000010002583: C3                 ret
  0000000010002584: 55                 push        rbp
  0000000010002585: 48 8B EC           mov         rbp,rsp
  0000000010002588: 48 83 EC 40        sub         rsp,40h
  000000001000258C: 48 89 4D 10        mov         qword ptr [rbp+10h],rcx
  0000000010002590: 48 83 EC 20        sub         rsp,20h
  0000000010002594: 48 8B 4D 10        mov         rcx,qword ptr [rbp+10h]
  0000000010002598: 48 8D 55 C0        lea         rdx,[rbp-40h]
  000000001000259C: 41 B8 20 00 00 00  mov         r8d,20h
  00000000100025A2: E8 2A FC FF FF     call        00000000100021D1
  00000000100025A7: 48 83 C4 20        add         rsp,20h
  00000000100025AB: 48 83 EC 20        sub         rsp,20h
  00000000100025AF: 48 8D 4D C0        lea         rcx,[rbp-40h]
  00000000100025B3: BA 20 00 00 00     mov         edx,20h
  00000000100025B8: E8 7B FB FF FF     call        0000000010002138
  00000000100025BD: 48 83 C4 20        add         rsp,20h
  00000000100025C1: C9                 leave
  00000000100025C2: C3                 ret
  00000000100025C3: 55                 push        rbp
  00000000100025C4: 48 8B EC           mov         rbp,rsp
  00000000100025C7: 48 81 EC 80 00 00  sub         rsp,80h
                    00
  00000000100025CE: 66 0F 7E 45 10     movd        dword ptr [rbp+10h],xmm0
  00000000100025D3: 48 89 55 18        mov         qword ptr [rbp+18h],rdx
  00000000100025D7: 4C 89 45 20        mov         qword ptr [rbp+20h],r8
  00000000100025DB: 31 C0              xor         eax,eax
  00000000100025DD: 48 8B 4D 18        mov         rcx,qword ptr [rbp+18h]
  00000000100025E1: 66 89 01           mov         word ptr [rcx],ax
  00000000100025E4: 8B 45 10           mov         eax,dword ptr [rbp+10h]
  00000000100025E7: 48 C1 F8 17        sar         rax,17h
  00000000100025EB: 48 81 E0 FF 00 00  and         rax,0FFh
                    00
  00000000100025F2: 48 83 E8 7F        sub         rax,7Fh
  00000000100025F6: 0F 80 05 00 00 00  jo          0000000010002601
  00000000100025FC: E9 08 00 00 00     jmp         0000000010002609
  0000000010002601: B0 00              mov         al,0
  0000000010002603: B9 BA 07 00 00     mov         ecx,7BAh
  0000000010002608: CC                 int         3
  0000000010002609: 48 89 45 E8        mov         qword ptr [rbp-18h],rax
  000000001000260D: 48 8B 45 E8        mov         rax,qword ptr [rbp-18h]
  0000000010002611: 48 83 F8 82        cmp         rax,0FFFFFFFFFFFFFF82h
  0000000010002615: 0F 8C FF 04 00 00  jl          0000000010002B1A
  000000001000261B: 48 8B 45 E8        mov         rax,qword ptr [rbp-18h]
  000000001000261F: 48 83 F8 7F        cmp         rax,7Fh
  0000000010002623: 0F 8F F1 04 00 00  jg          0000000010002B1A
  0000000010002629: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  000000001000262E: 0F 57 C9           xorps       xmm1,xmm1
  0000000010002631: 0F 2F C1           comiss      xmm0,xmm1
  0000000010002634: 0F 92 C0           setb        al
  0000000010002637: 0F B6 C0           movzx       eax,al
  000000001000263A: 88 45 CF           mov         byte ptr [rbp-31h],al
  000000001000263D: 0F B6 45 CF        movzx       eax,byte ptr [rbp-31h]
  0000000010002641: 85 C0              test        eax,eax
  0000000010002643: 0F 84 37 00 00 00  je          0000000010002680
  0000000010002649: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  000000001000264E: 0F 57 C9           xorps       xmm1,xmm1
  0000000010002651: F3 0F 5C C8        subss       xmm1,xmm0
  0000000010002655: F3 0F 10 C1        movss       xmm0,xmm1
  0000000010002659: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  000000001000265E: 48 83 EC 20        sub         rsp,20h
  0000000010002662: 48 8D 0D 8F F8 FF  lea         rcx,[10001EF8h]
                    FF
  0000000010002669: BA 02 00 00 00     mov         edx,2
  000000001000266E: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002672: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002676: FF 15 AC F8 FF FF  call        qword ptr [10001F28h]
  000000001000267C: 48 83 C4 20        add         rsp,20h
  0000000010002680: 31 C0              xor         eax,eax
  0000000010002682: 48 89 45 E0        mov         qword ptr [rbp-20h],rax
  0000000010002686: 31 C0              xor         eax,eax
  0000000010002688: 66 89 45 CC        mov         word ptr [rbp-34h],ax
  000000001000268C: 48 8B 45 E8        mov         rax,qword ptr [rbp-18h]
  0000000010002690: 48 83 F8 00        cmp         rax,0
  0000000010002694: 0F 8E 3D 00 00 00  jle         00000000100026D7
  000000001000269A: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  000000001000269F: B8 00 00 20 41     mov         eax,41200000h
  00000000100026A4: 66 0F 6E C8        movd        xmm1,eax
  00000000100026A8: 0F 2F C1           comiss      xmm0,xmm1
  00000000100026AB: 0F 86 21 00 00 00  jbe         00000000100026D2
  00000000100026B1: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100026B6: B8 00 00 20 41     mov         eax,41200000h
  00000000100026BB: 66 0F 6E C8        movd        xmm1,eax
  00000000100026BF: F3 0F 5E C1        divss       xmm0,xmm1
  00000000100026C3: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  00000000100026C8: 48 83 45 E0 01     add         qword ptr [rbp-20h],1
  00000000100026CD: E9 C8 FF FF FF     jmp         000000001000269A
  00000000100026D2: E9 46 00 00 00     jmp         000000001000271D
  00000000100026D7: 48 8B 45 E8        mov         rax,qword ptr [rbp-18h]
  00000000100026DB: 48 83 F8 00        cmp         rax,0
  00000000100026DF: 0F 8D 38 00 00 00  jge         000000001000271D
  00000000100026E5: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100026EA: B8 00 00 80 3F     mov         eax,3F800000h
  00000000100026EF: 66 0F 6E C8        movd        xmm1,eax
  00000000100026F3: 0F 2F C1           comiss      xmm0,xmm1
  00000000100026F6: 0F 83 21 00 00 00  jae         000000001000271D
  00000000100026FC: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002701: B8 00 00 20 41     mov         eax,41200000h
  0000000010002706: 66 0F 6E C8        movd        xmm1,eax
  000000001000270A: F3 0F 59 C1        mulss       xmm0,xmm1
  000000001000270E: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  0000000010002713: 48 83 45 E0 FF     add         qword ptr [rbp-20h],0FFFFFFFFFFFFFFFFh
  0000000010002718: E9 C8 FF FF FF     jmp         00000000100026E5
  000000001000271D: 48 83 EC 08        sub         rsp,8
  0000000010002721: 0F AE 1C 24        stmxcsr     dword ptr [rsp]
  0000000010002725: 0F BA 2C 24 0D     bts         dword ptr [rsp],0Dh
  000000001000272A: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  000000001000272E: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002733: F3 48 0F 2D C0     cvtss2si    rax,xmm0
  0000000010002738: 0F BA 34 24 0D     btr         dword ptr [rsp],0Dh
  000000001000273D: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  0000000010002741: 48 83 C4 08        add         rsp,8
  0000000010002745: 48 89 45 F0        mov         qword ptr [rbp-10h],rax
  0000000010002749: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  000000001000274E: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  0000000010002752: F3 48 0F 2A C8     cvtsi2ss    xmm1,rax
  0000000010002757: F3 0F 5C C1        subss       xmm0,xmm1
  000000001000275B: B8 00 00 20 41     mov         eax,41200000h
  0000000010002760: 66 0F 6E C8        movd        xmm1,eax
  0000000010002764: F3 0F 59 C1        mulss       xmm0,xmm1
  0000000010002768: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  000000001000276D: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  0000000010002771: 48 83 C0 30        add         rax,30h
  0000000010002775: 0F 80 05 00 00 00  jo          0000000010002780
  000000001000277B: E9 08 00 00 00     jmp         0000000010002788
  0000000010002780: B0 00              mov         al,0
  0000000010002782: B9 29 09 00 00     mov         ecx,929h
  0000000010002787: CC                 int         3
  0000000010002788: 66 89 45 CA        mov         word ptr [rbp-36h],ax
  000000001000278C: 48 83 EC 20        sub         rsp,20h
  0000000010002790: 48 8D 4D CA        lea         rcx,[rbp-36h]
  0000000010002794: BA 02 00 00 00     mov         edx,2
  0000000010002799: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  000000001000279D: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  00000000100027A1: FF 15 81 F7 FF FF  call        qword ptr [10001F28h]
  00000000100027A7: 48 83 C4 20        add         rsp,20h
  00000000100027AB: 48 83 EC 20        sub         rsp,20h
  00000000100027AF: 48 8D 0D 38 F7 FF  lea         rcx,[10001EEEh]
                    FF
  00000000100027B6: BA 02 00 00 00     mov         edx,2
  00000000100027BB: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  00000000100027BF: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  00000000100027C3: FF 15 5F F7 FF FF  call        qword ptr [10001F28h]
  00000000100027C9: 48 83 C4 20        add         rsp,20h
  00000000100027CD: 31 C0              xor         eax,eax
  00000000100027CF: 48 89 45 D8        mov         qword ptr [rbp-28h],rax
  00000000100027D3: 31 C0              xor         eax,eax
  00000000100027D5: 48 89 45 F8        mov         qword ptr [rbp-8],rax
  00000000100027D9: 31 C0              xor         eax,eax
  00000000100027DB: 48 89 45 D0        mov         qword ptr [rbp-30h],rax
  00000000100027DF: 48 8B 45 F8        mov         rax,qword ptr [rbp-8]
  00000000100027E3: 48 83 F8 06        cmp         rax,6
  00000000100027E7: 0F 8D CD 00 00 00  jge         00000000100028BA
  00000000100027ED: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100027F2: 0F 57 C9           xorps       xmm1,xmm1
  00000000100027F5: 0F 2F C1           comiss      xmm0,xmm1
  00000000100027F8: 0F 84 BC 00 00 00  je          00000000100028BA
  00000000100027FE: 48 83 45 F8 01     add         qword ptr [rbp-8],1
  0000000010002803: 48 83 EC 08        sub         rsp,8
  0000000010002807: 0F AE 1C 24        stmxcsr     dword ptr [rsp]
  000000001000280B: 0F BA 2C 24 0D     bts         dword ptr [rsp],0Dh
  0000000010002810: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  0000000010002814: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002819: F3 48 0F 2D C0     cvtss2si    rax,xmm0
  000000001000281E: 0F BA 34 24 0D     btr         dword ptr [rsp],0Dh
  0000000010002823: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  0000000010002827: 48 83 C4 08        add         rsp,8
  000000001000282B: 48 89 45 F0        mov         qword ptr [rbp-10h],rax
  000000001000282F: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002834: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  0000000010002838: F3 48 0F 2A C8     cvtsi2ss    xmm1,rax
  000000001000283D: F3 0F 5C C1        subss       xmm0,xmm1
  0000000010002841: B8 00 00 20 41     mov         eax,41200000h
  0000000010002846: 66 0F 6E C8        movd        xmm1,eax
  000000001000284A: F3 0F 59 C1        mulss       xmm0,xmm1
  000000001000284E: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  0000000010002853: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  0000000010002857: 48 83 F8 00        cmp         rax,0
  000000001000285B: 0F 85 0E 00 00 00  jne         000000001000286F
  0000000010002861: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  0000000010002865: 48 83 F8 00        cmp         rax,0
  0000000010002869: 0F 84 41 00 00 00  je          00000000100028B0
  000000001000286F: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  0000000010002873: 48 69 C0 0A 00 00  imul        rax,rax,0Ah
                    00
  000000001000287A: 0F 80 05 00 00 00  jo          0000000010002885
  0000000010002880: E9 08 00 00 00     jmp         000000001000288D
  0000000010002885: B0 00              mov         al,0
  0000000010002887: B9 14 0A 00 00     mov         ecx,0A14h
  000000001000288C: CC                 int         3
  000000001000288D: 48 8B 4D F0        mov         rcx,qword ptr [rbp-10h]
  0000000010002891: 48 03 C1           add         rax,rcx
  0000000010002894: 0F 80 05 00 00 00  jo          000000001000289F
  000000001000289A: E9 08 00 00 00     jmp         00000000100028A7
  000000001000289F: B0 00              mov         al,0
  00000000100028A1: B9 1F 0A 00 00     mov         ecx,0A1Fh
  00000000100028A6: CC                 int         3
  00000000100028A7: 48 89 45 D8        mov         qword ptr [rbp-28h],rax
  00000000100028AB: E9 05 00 00 00     jmp         00000000100028B5
  00000000100028B0: 48 83 45 D0 01     add         qword ptr [rbp-30h],1
  00000000100028B5: E9 25 FF FF FF     jmp         00000000100027DF
  00000000100028BA: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100028BF: 0F 57 C9           xorps       xmm1,xmm1
  00000000100028C2: 0F 2F C1           comiss      xmm0,xmm1
  00000000100028C5: 0F 84 CA 00 00 00  je          0000000010002995
  00000000100028CB: 48 83 EC 08        sub         rsp,8
  00000000100028CF: 0F AE 1C 24        stmxcsr     dword ptr [rsp]
  00000000100028D3: 0F BA 2C 24 0D     bts         dword ptr [rsp],0Dh
  00000000100028D8: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  00000000100028DC: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100028E1: F3 48 0F 2D C0     cvtss2si    rax,xmm0
  00000000100028E6: 0F BA 34 24 0D     btr         dword ptr [rsp],0Dh
  00000000100028EB: 0F AE 14 24        ldmxcsr     dword ptr [rsp]
  00000000100028EF: 48 83 C4 08        add         rsp,8
  00000000100028F3: 48 89 45 F0        mov         qword ptr [rbp-10h],rax
  00000000100028F7: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  00000000100028FC: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  0000000010002900: F3 48 0F 2A C8     cvtsi2ss    xmm1,rax
  0000000010002905: F3 0F 5C C1        subss       xmm0,xmm1
  0000000010002909: F3 0F 11 45 10     movss       dword ptr [rbp+10h],xmm0
  000000001000290E: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002913: B8 00 00 00 3F     mov         eax,3F000000h
  0000000010002918: 66 0F 6E C8        movd        xmm1,eax
  000000001000291C: 0F 2F C1           comiss      xmm0,xmm1
  000000001000291F: 0F 87 21 00 00 00  ja          0000000010002946
  0000000010002925: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  000000001000292A: B8 00 00 00 3F     mov         eax,3F000000h
  000000001000292F: 66 0F 6E C8        movd        xmm1,eax
  0000000010002933: 0F 2F C1           comiss      xmm0,xmm1
  0000000010002936: 0F 85 0F 00 00 00  jne         000000001000294B
  000000001000293C: 48 83 E0 01        and         rax,1
  0000000010002940: 0F 84 05 00 00 00  je          000000001000294B
  0000000010002946: 48 83 45 F0 01     add         qword ptr [rbp-10h],1
  000000001000294B: 48 8B 45 F0        mov         rax,qword ptr [rbp-10h]
  000000001000294F: 48 83 F8 00        cmp         rax,0
  0000000010002953: 0F 84 3C 00 00 00  je          0000000010002995
  0000000010002959: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  000000001000295D: 48 69 C0 0A 00 00  imul        rax,rax,0Ah
                    00
  0000000010002964: 0F 80 05 00 00 00  jo          000000001000296F
  000000001000296A: E9 08 00 00 00     jmp         0000000010002977
  000000001000296F: B0 00              mov         al,0
  0000000010002971: B9 D2 0A 00 00     mov         ecx,0AD2h
  0000000010002976: CC                 int         3
  0000000010002977: 48 8B 4D F0        mov         rcx,qword ptr [rbp-10h]
  000000001000297B: 48 03 C1           add         rax,rcx
  000000001000297E: 0F 80 05 00 00 00  jo          0000000010002989
  0000000010002984: E9 08 00 00 00     jmp         0000000010002991
  0000000010002989: B0 00              mov         al,0
  000000001000298B: B9 D8 0A 00 00     mov         ecx,0AD8h
  0000000010002990: CC                 int         3
  0000000010002991: 48 89 45 D8        mov         qword ptr [rbp-28h],rax
  0000000010002995: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  0000000010002999: B9 0A 00 00 00     mov         ecx,0Ah
  000000001000299E: 48 85 C9           test        rcx,rcx
  00000000100029A1: 0F 8E 05 00 00 00  jle         00000000100029AC
  00000000100029A7: E9 08 00 00 00     jmp         00000000100029B4
  00000000100029AC: B0 00              mov         al,0
  00000000100029AE: B9 F7 0A 00 00     mov         ecx,0AF7h
  00000000100029B3: CC                 int         3
  00000000100029B4: 48 99              cqo
  00000000100029B6: 48 85 C0           test        rax,rax
  00000000100029B9: 0F 88 08 00 00 00  js          00000000100029C7
  00000000100029BF: 48 F7 F9           idiv        rax,rcx
  00000000100029C2: E9 0F 00 00 00     jmp         00000000100029D6
  00000000100029C7: 48 F7 F9           idiv        rax,rcx
  00000000100029CA: 48 85 D2           test        rdx,rdx
  00000000100029CD: 0F 84 03 00 00 00  je          00000000100029D6
  00000000100029D3: 48 03 D1           add         rdx,rcx
  00000000100029D6: 48 8B C2           mov         rax,rdx
  00000000100029D9: 48 83 F8 00        cmp         rax,0
  00000000100029DD: 0F 85 4B 00 00 00  jne         0000000010002A2E
  00000000100029E3: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  00000000100029E7: B9 0A 00 00 00     mov         ecx,0Ah
  00000000100029EC: 48 85 C9           test        rcx,rcx
  00000000100029EF: 0F 8E 05 00 00 00  jle         00000000100029FA
  00000000100029F5: E9 08 00 00 00     jmp         0000000010002A02
  00000000100029FA: B0 00              mov         al,0
  00000000100029FC: B9 16 0B 00 00     mov         ecx,0B16h
  0000000010002A01: CC                 int         3
  0000000010002A02: 48 99              cqo
  0000000010002A04: 48 85 C0           test        rax,rax
  0000000010002A07: 0F 88 08 00 00 00  js          0000000010002A15
  0000000010002A0D: 48 F7 F9           idiv        rax,rcx
  0000000010002A10: E9 10 00 00 00     jmp         0000000010002A25
  0000000010002A15: 48 F7 F9           idiv        rax,rcx
  0000000010002A18: 48 85 D2           test        rdx,rdx
  0000000010002A1B: 0F 84 04 00 00 00  je          0000000010002A25
  0000000010002A21: 48 83 E8 01        sub         rax,1
  0000000010002A25: 48 89 45 D8        mov         qword ptr [rbp-28h],rax
  0000000010002A29: E9 67 FF FF FF     jmp         0000000010002995
  0000000010002A2E: 48 8B 45 D0        mov         rax,qword ptr [rbp-30h]
  0000000010002A32: 48 83 F8 00        cmp         rax,0
  0000000010002A36: 0F 8E 2C 00 00 00  jle         0000000010002A68
  0000000010002A3C: 48 83 EC 20        sub         rsp,20h
  0000000010002A40: 48 8D 0D A3 F4 FF  lea         rcx,[10001EEAh]
                    FF
  0000000010002A47: BA 02 00 00 00     mov         edx,2
  0000000010002A4C: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002A50: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002A54: FF 15 CE F4 FF FF  call        qword ptr [10001F28h]
  0000000010002A5A: 48 83 C4 20        add         rsp,20h
  0000000010002A5E: 48 83 45 D0 FF     add         qword ptr [rbp-30h],0FFFFFFFFFFFFFFFFh
  0000000010002A63: E9 C6 FF FF FF     jmp         0000000010002A2E
  0000000010002A68: 48 8B 45 D8        mov         rax,qword ptr [rbp-28h]
  0000000010002A6C: 48 83 F8 00        cmp         rax,0
  0000000010002A70: 0F 84 3A 00 00 00  je          0000000010002AB0
  0000000010002A76: 48 83 EC 20        sub         rsp,20h
  0000000010002A7A: 48 8B 4D D8        mov         rcx,qword ptr [rbp-28h]
  0000000010002A7E: 48 8D 55 8A        lea         rdx,[rbp-76h]
  0000000010002A82: 41 B8 20 00 00 00  mov         r8d,20h
  0000000010002A88: E8 44 F7 FF FF     call        00000000100021D1
  0000000010002A8D: 48 83 C4 20        add         rsp,20h
  0000000010002A91: 48 83 EC 20        sub         rsp,20h
  0000000010002A95: 48 8D 4D 8A        lea         rcx,[rbp-76h]
  0000000010002A99: BA 20 00 00 00     mov         edx,20h
  0000000010002A9E: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002AA2: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002AA6: FF 15 7C F4 FF FF  call        qword ptr [10001F28h]
  0000000010002AAC: 48 83 C4 20        add         rsp,20h
  0000000010002AB0: 48 8B 45 E0        mov         rax,qword ptr [rbp-20h]
  0000000010002AB4: 48 83 F8 00        cmp         rax,0
  0000000010002AB8: 0F 84 5C 00 00 00  je          0000000010002B1A
  0000000010002ABE: 48 83 EC 20        sub         rsp,20h
  0000000010002AC2: 48 8D 0D 1D F4 FF  lea         rcx,[10001EE6h]
                    FF
  0000000010002AC9: BA 02 00 00 00     mov         edx,2
  0000000010002ACE: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002AD2: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002AD6: FF 15 4C F4 FF FF  call        qword ptr [10001F28h]
  0000000010002ADC: 48 83 C4 20        add         rsp,20h
  0000000010002AE0: 48 83 EC 20        sub         rsp,20h
  0000000010002AE4: 48 8B 4D E0        mov         rcx,qword ptr [rbp-20h]
  0000000010002AE8: 48 8D 55 8A        lea         rdx,[rbp-76h]
  0000000010002AEC: 41 B8 20 00 00 00  mov         r8d,20h
  0000000010002AF2: E8 DA F6 FF FF     call        00000000100021D1
  0000000010002AF7: 48 83 C4 20        add         rsp,20h
  0000000010002AFB: 48 83 EC 20        sub         rsp,20h
  0000000010002AFF: 48 8D 4D 8A        lea         rcx,[rbp-76h]
  0000000010002B03: BA 20 00 00 00     mov         edx,20h
  0000000010002B08: 4C 8B 45 18        mov         r8,qword ptr [rbp+18h]
  0000000010002B0C: 4C 8B 4D 20        mov         r9,qword ptr [rbp+20h]
  0000000010002B10: FF 15 12 F4 FF FF  call        qword ptr [10001F28h]
  0000000010002B16: 48 83 C4 20        add         rsp,20h
  0000000010002B1A: C9                 leave
  0000000010002B1B: C3                 ret
  0000000010002B1C: 55                 push        rbp
  0000000010002B1D: 48 8B EC           mov         rbp,rsp
  0000000010002B20: 48 83 EC 40        sub         rsp,40h
  0000000010002B24: 66 0F 7E 45 10     movd        dword ptr [rbp+10h],xmm0
  0000000010002B29: 48 83 EC 20        sub         rsp,20h
  0000000010002B2D: F3 0F 10 45 10     movss       xmm0,dword ptr [rbp+10h]
  0000000010002B32: 48 8D 55 C0        lea         rdx,[rbp-40h]
  0000000010002B36: 41 B8 20 00 00 00  mov         r8d,20h
  0000000010002B3C: E8 82 FA FF FF     call        00000000100025C3
  0000000010002B41: 48 83 C4 20        add         rsp,20h
  0000000010002B45: 48 83 EC 20        sub         rsp,20h
  0000000010002B49: 48 8D 4D C0        lea         rcx,[rbp-40h]
  0000000010002B4D: BA 20 00 00 00     mov         edx,20h
  0000000010002B52: E8 E1 F5 FF FF     call        0000000010002138
  0000000010002B57: 48 83 C4 20        add         rsp,20h
  0000000010002B5B: C9                 leave
  0000000010002B5C: C3                 ret
  0000000010002B5D: 55                 push        rbp
  0000000010002B5E: 48 8B EC           mov         rbp,rsp
  0000000010002B61: 48 83 EC 10        sub         rsp,10h
  0000000010002B65: 48 89 4D 10        mov         qword ptr [rbp+10h],rcx
  0000000010002B69: 48 83 EC 30        sub         rsp,30h
  0000000010002B6D: 48 8B 0D 64 F3 FF  mov         rcx,qword ptr [10001ED8h]
                    FF
  0000000010002B74: 48 8B 09           mov         rcx,qword ptr [rcx]
  0000000010002B77: 51                 push        rcx
  0000000010002B78: 48 8B 0D 79 F4 FF  mov         rcx,qword ptr [10001FF8h]
                    FF
  0000000010002B7F: 51                 push        rcx
  0000000010002B80: 48 83 EC 20        sub         rsp,20h
  0000000010002B84: 48 8B 4D 10        mov         rcx,qword ptr [rbp+10h]
  0000000010002B88: BA 02 00 00 00     mov         edx,2
  0000000010002B8D: FF 15 BD F3 FF FF  call        qword ptr [10001F50h]
  0000000010002B93: 48 83 C4 20        add         rsp,20h
  0000000010002B97: 59                 pop         rcx
  0000000010002B98: 48 8B D0           mov         rdx,rax
  0000000010002B9B: 41 B8 01 00 00 00  mov         r8d,1
  0000000010002BA1: 51                 push        rcx
  0000000010002BA2: 52                 push        rdx
  0000000010002BA3: 41 50              push        r8
  0000000010002BA5: 48 83 EC 20        sub         rsp,20h
  0000000010002BA9: 48 8D 4D FC        lea         rcx,[rbp-4]
  0000000010002BAD: BA 04 00 00 00     mov         edx,4
  0000000010002BB2: FF 15 98 F3 FF FF  call        qword ptr [10001F50h]
  0000000010002BB8: 48 83 C4 20        add         rsp,20h
  0000000010002BBC: 41 58              pop         r8
  0000000010002BBE: 5A                 pop         rdx
  0000000010002BBF: 59                 pop         rcx
  0000000010002BC0: 4C 8B C8           mov         r9,rax
  0000000010002BC3: 48 8B 05 1E F4 FF  mov         rax,qword ptr [10001FE8h]
                    FF
  0000000010002BCA: 48 89 44 24 28     mov         qword ptr [rsp+28h],rax
  0000000010002BCF: 58                 pop         rax
  0000000010002BD0: FF D0              call        rax
  0000000010002BD2: 48 83 C4 30        add         rsp,30h
  0000000010002BD6: 89 45 F8           mov         dword ptr [rbp-8],eax
  0000000010002BD9: 48 83 EC 20        sub         rsp,20h
  0000000010002BDD: 8B 4D F8           mov         ecx,dword ptr [rbp-8]
  0000000010002BE0: FF 15 62 F3 FF FF  call        qword ptr [10001F48h]
  0000000010002BE6: 48 83 C4 20        add         rsp,20h
  0000000010002BEA: 8B 4D FC           mov         ecx,dword ptr [rbp-4]
  0000000010002BED: 83 F9 01           cmp         ecx,1
  0000000010002BF0: C9                 leave
  0000000010002BF1: C3                 ret
  0000000010002BF2: 55                 push        rbp
  0000000010002BF3: 48 8B EC           mov         rbp,rsp
  0000000010002BF6: 48 83 EC 10        sub         rsp,10h
  0000000010002BFA: 31 C0              xor         eax,eax
  0000000010002BFC: 48 89 05 E5 F3 FF  mov         qword ptr [10001FE8h],rax
                    FF
  0000000010002C03: 48 83 EC 20        sub         rsp,20h
  0000000010002C07: 48 8B 0D C2 F2 FF  mov         rcx,qword ptr [10001ED0h]
                    FF
  0000000010002C0E: 48 8B 09           mov         rcx,qword ptr [rcx]
  0000000010002C11: 51                 push        rcx
  0000000010002C12: 58                 pop         rax
  0000000010002C13: FF D0              call        rax
  0000000010002C15: 48 83 C4 20        add         rsp,20h
  0000000010002C19: 89 45 FC           mov         dword ptr [rbp-4],eax
  0000000010002C1C: 48 83 EC 20        sub         rsp,20h
  0000000010002C20: 48 8B 0D A1 F2 FF  mov         rcx,qword ptr [10001EC8h]
                    FF
  0000000010002C27: 48 8B 09           mov         rcx,qword ptr [rcx]
  0000000010002C2A: 51                 push        rcx
  0000000010002C2B: 48 B9 F6 FF FF FF  mov         rcx,0FFFFFFFFFFFFFFF6h
                    FF FF FF FF
  0000000010002C35: 58                 pop         rax
  0000000010002C36: FF D0              call        rax
  0000000010002C38: 48 83 C4 20        add         rsp,20h
  0000000010002C3C: 48 89 05 B5 F3 FF  mov         qword ptr [10001FF8h],rax
                    FF
  0000000010002C43: 48 83 EC 20        sub         rsp,20h
  0000000010002C47: 48 8B 0D 7A F2 FF  mov         rcx,qword ptr [10001EC8h]
                    FF
  0000000010002C4E: 48 8B 09           mov         rcx,qword ptr [rcx]
  0000000010002C51: 51                 push        rcx
  0000000010002C52: 48 B9 F5 FF FF FF  mov         rcx,0FFFFFFFFFFFFFFF5h
                    FF FF FF FF
  0000000010002C5C: 58                 pop         rax
  0000000010002C5D: FF D0              call        rax
  0000000010002C5F: 48 83 C4 20        add         rsp,20h
  0000000010002C63: 48 89 05 86 F3 FF  mov         qword ptr [10001FF0h],rax
                    FF
  0000000010002C6A: C9                 leave
  0000000010002C6B: C3                 ret
  0000000010002C6C: 83 FA 01           cmp         edx,1
  0000000010002C6F: 0F 84 01 00 00 00  je          0000000010002C76
  0000000010002C75: C3                 ret
  0000000010002C76: 48 83 EC 28        sub         rsp,28h
  0000000010002C7A: FF 15 40 F3 FF FF  call        qword ptr [10001FC0h]
  0000000010002C80: 48 89 05 19 F3 FF  mov         qword ptr [10001FA0h],rax
                    FF
  0000000010002C87: 48 8D 0D 24 F2 FF  lea         rcx,[10001EB2h]
                    FF
  0000000010002C8E: FF 15 1C F3 FF FF  call        qword ptr [10001FB0h]
  0000000010002C94: 48 8B F0           mov         rsi,rax
  0000000010002C97: 48 B8 96 2A 6A 39  mov         rax,88FDAF70396A2A96h
                    70 AF FD 88
  0000000010002CA1: 48 3B 86 F0 03 00  cmp         rax,qword ptr [rsi+000003F0h]
                    00
  0000000010002CA8: 0F 85 05 00 00 00  jne         0000000010002CB3
  0000000010002CAE: E9 08 00 00 00     jmp         0000000010002CBB
  0000000010002CB3: B0 05              mov         al,5
  0000000010002CB5: B9 74 0E 00 00     mov         ecx,0E74h
  0000000010002CBA: CC                 int         3
  0000000010002CBB: 48 B8 25 35 A6 71  mov         rax,3907FDC571A63525h
                    C5 FD 07 39
  0000000010002CC5: 48 3B 86 F8 03 00  cmp         rax,qword ptr [rsi+000003F8h]
                    00
  0000000010002CCC: 0F 85 05 00 00 00  jne         0000000010002CD7
  0000000010002CD2: E9 08 00 00 00     jmp         0000000010002CDF
  0000000010002CD7: B0 05              mov         al,5
  0000000010002CD9: B9 74 0E 00 00     mov         ecx,0E74h
  0000000010002CDE: CC                 int         3
  0000000010002CDF: 48 8B CE           mov         rcx,rsi
  0000000010002CE2: BA 07 00 00 00     mov         edx,7
  0000000010002CE7: FF 15 CB F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002CED: 48 89 05 DC F1 FF  mov         qword ptr [10001ED0h],rax
                    FF
  0000000010002CF4: 48 8B CE           mov         rcx,rsi
  0000000010002CF7: BA 08 00 00 00     mov         edx,8
  0000000010002CFC: FF 15 B6 F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002D02: 48 89 05 BF F1 FF  mov         qword ptr [10001EC8h],rax
                    FF
  0000000010002D09: 48 8B CE           mov         rcx,rsi
  0000000010002D0C: BA 09 00 00 00     mov         edx,9
  0000000010002D11: FF 15 A1 F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002D17: 48 89 05 3A F2 FF  mov         qword ptr [10001F58h],rax
                    FF
  0000000010002D1E: 48 8B CE           mov         rcx,rsi
  0000000010002D21: BA 0A 00 00 00     mov         edx,0Ah
  0000000010002D26: FF 15 8C F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002D2C: 48 89 05 A5 F1 FF  mov         qword ptr [10001ED8h],rax
                    FF
  0000000010002D33: 48 8B CE           mov         rcx,rsi
  0000000010002D36: BA 1A 00 00 00     mov         edx,1Ah
  0000000010002D3B: FF 15 77 F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002D41: 48 89 05 08 F2 FF  mov         qword ptr [10001F50h],rax
                    FF
  0000000010002D48: 48 8B CE           mov         rcx,rsi
  0000000010002D4B: BA 1B 00 00 00     mov         edx,1Bh
  0000000010002D50: FF 15 62 F2 FF FF  call        qword ptr [10001FB8h]
  0000000010002D56: 48 89 05 EB F1 FF  mov         qword ptr [10001F48h],rax
                    FF
  0000000010002D5D: 48 8D 0D 36 F1 FF  lea         rcx,[10001E9Ah]
                    FF
  0000000010002D64: FF 15 46 F2 FF FF  call        qword ptr [10001FB0h]
  0000000010002D6A: 48 8B F0           mov         rsi,rax
  0000000010002D6D: 48 B8 A0 49 0E 8E  mov         rax,0B9484CB58E0E49A0h
                    B5 4C 48 B9
  0000000010002D77: 48 3B 86 F0 03 00  cmp         rax,qword ptr [rsi+000003F0h]
                    00
  0000000010002D7E: 0F 85 05 00 00 00  jne         0000000010002D89
  0000000010002D84: E9 08 00 00 00     jmp         0000000010002D91
  0000000010002D89: B0 05              mov         al,5
  0000000010002D8B: B9 74 0E 00 00     mov         ecx,0E74h
  0000000010002D90: CC                 int         3
  0000000010002D91: 48 B8 E6 CD 64 42  mov         rax,53460D024264CDE6h
                    02 0D 46 53
  0000000010002D9B: 48 3B 86 F8 03 00  cmp         rax,qword ptr [rsi+000003F8h]
                    00
  0000000010002DA2: 0F 85 05 00 00 00  jne         0000000010002DAD
  0000000010002DA8: E9 08 00 00 00     jmp         0000000010002DB5
  0000000010002DAD: B0 05              mov         al,5
  0000000010002DAF: B9 74 0E 00 00     mov         ecx,0E74h
  0000000010002DB4: CC                 int         3
  0000000010002DB5: 48 8B CE           mov         rcx,rsi
  0000000010002DB8: BA 01 00 00 00     mov         edx,1
  0000000010002DBD: FF 15 F5 F1 FF FF  call        qword ptr [10001FB8h]
  0000000010002DC3: 48 89 05 76 F1 FF  mov         qword ptr [10001F40h],rax
                    FF
  0000000010002DCA: 48 8B CE           mov         rcx,rsi
  0000000010002DCD: BA 03 00 00 00     mov         edx,3
  0000000010002DD2: FF 15 E0 F1 FF FF  call        qword ptr [10001FB8h]
  0000000010002DD8: 48 89 05 49 F1 FF  mov         qword ptr [10001F28h],rax
                    FF
  0000000010002DDF: 48 83 EC 20        sub         rsp,20h
  0000000010002DE3: E8 0A FE FF FF     call        0000000010002BF2
  0000000010002DE8: 48 83 C4 20        add         rsp,20h
  0000000010002DEC: B8 01 00 00 00     mov         eax,1
  0000000010002DF1: 48 83 C4 28        add         rsp,28h
  0000000010002DF5: C3                 ret

  Summary

        1000 .data
        1000 .edata
        1000 .idata
        1000 .reloc
        1000 .text
