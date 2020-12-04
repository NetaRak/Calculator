section .rodata
    overflow:               db      "Error: Operand Stack Overflow",10,0
    insuf:                  db      "Error: Insufficient Number of Arguments on Stack",10,0
    calc:                   db      "calc: ",0,0
    newLine:                db      10,0
    zero:                   db      "0",0,0
    quit:                   db      "I quit",0,0
    format:                 db     "%s",0,0 ; printf string format
    formatStringNewLine:    db     "%s",10,0 ; printf string format
    formatD:                db     "%d",0,0 ; printf string format
    printf_f:               db     "%X",0,0 ; printf hexadecimal number format no \n
    format_0:               db     "%X",0,0 ; printf hexadecimal number format no \n
    format_p0:              db     "0%X",0,0 ; printf hexadecimal number format no \n
    printf_hNewLine:        db     "%X",10,0 ; printf hexadecimal number format + \n
   
section .data
    debug:              db      0         ; debug mode flag
    data:               db      0        ; input data (2 bytes)                     db      0
    dformat:            db     "DEBUG: %s",10,0
    count:              dd      0         ; Number of operations 
    isEvenLen:          dd      0         ; Indicates the length of buffer opeator is even(0) or odd(1)
    isFirst:            dd      0           ; Indicates the length of buffer opeator is even(0) or odd(1)
    isFirstDop:         dd      0
    odd:                dd      0         ; Indicates if the input operand has odd number of characters
    btmp:               db      0         ; assistant variable
    dtmp:               dd      0         ; assistant variable
    sixteen:            dd      16
    two:                dd      2
    foundFirstNum:      dd      0
    start:              dd      0
    startAfter0:        dd      0
    nodePtr:            dd         0 
    isNodeCreated:      dd      0
    
section .bss
    stackSize:                  resd   1
    stackCounter:               resd   80
    buffer:                     resd   82  
    digCount:                   resd   1
    print0_Pointer:             resb   1
    pointerToLastLink:          resd   1
    HelpAddPointer:             resd   1
    dupPointer:                 resd   1
    firstEDX:                   resd   1
    isFirstPrint:               resd   1
    numToCopy:                  resd   1
    numOfDig_FreePointer:       resd   1
    popAndPrint_FreePointer:    resd   1
    and_FreePointer:            resd   1
    sum_FreePointer:            resd   1
    sumCopy_FreePointer:            resd   1
    or_FreePointer:             resd   1
    opStack:                    resd   5         ; Operands stack array
    

section .text
  align 16
  global main
  extern stdin
  extern printf
  extern fprintf 
  extern fflush
  extern malloc 
  extern calloc 
  extern free 
  extern gets 
  extern getchar 
  extern fgets 
  extern stderr





;-----------------------------------------MACROS-----------------------------------------------

%macro add_Link 0
    mov edx,eax
    mov eax,0
    push edx
    push 5
    push 1
    call calloc 
    add esp,8  
    pop edx  
    mov ecx,0
    mov ebx,0
    mov ecx,dword[stackCounter]
    mov ebx,dword[pointerToLastLink]                            ;calloc put address for the new link at eax
    mov byte[eax],dl                          ;put the numeric value in the link   
    mov dword[eax+1],ebx                     ;pointerToLastLink Initialized at 0                                          ;dword[eax+1] is the pionter to the next link  
    mov dword[opStack+4*ecx],eax                        ;Put the link in the right place in the stack   
    mov dword[pointerToLastLink],0            ;put a pointer to this link in pointerToLastLink so that we can link it to the next link we create                                    ;after push
    mov dword[pointerToLastLink],eax
    mov ecx,0
    mov ebx,0
%endmacro


%macro add_first_Link 0
mov dl,al
mov eax,0
push edx
push 5
push 1
call calloc 
add esp,8  
pop edx
mov ecx,0
mov ecx,dword[stackCounter]               ;calloc put address for the new link at eax
mov byte[eax],dl                          ;put the numeric value in the link   
mov dword[eax+1],0                        ;pointerToLastLink Initialized at 0                                          ;dword[eax+1] is the pionter to the next link  
mov dword[opStack+4*ecx],eax              ;Put the link in the right place in the stack   
mov dword[pointerToLastLink],eax
mov ecx,0
%endmacro

%macro print_opernd 0
     mov dword[isFirstPrint],0
     mov eax,edx
     mov edx,0
     mov ecx,0
     mov dword[print0_Pointer],eax

checkAll0:
     cmp eax,0
     je onlyZ
     mov cl,byte[eax]
     cmp cl,0
     jne notAll0
     mov eax,[eax+1]
     jmp checkAll0

notAll0:
     mov eax,dword[print0_Pointer]

start_print_opernd:
    cmp eax,0
    je print_print_opernd
    mov cl,byte[eax]

push_print_opernd:
    push ecx
    inc edx
    mov eax,[eax+1]
    jmp start_print_opernd

print_print_opernd:
beforeFirstnonZero:
    pop ebx
    cmp edx,0
    je endPrintOp
    cmp ebx,0
    je b2
    jmp b3

b2:
    dec edx
    jmp beforeFirstnonZero

b3:
    push ebx

afterFirstnonZero:
    pop ebx
    cmp edx,0
    je endPrintOp

cPrint:
    cmp dword[isFirstPrint],0
    je printFirst
    cmp ebx,16
    jl with0
    jmp withOut0

printFirst:
    mov dword[isFirstPrint],1
    jmp withOut0 

with0:
    dec edx
    pushad
    push ebx 
    push format_p0
    call printf
    add esp,8
    popad
    jmp afterFirstnonZero

withOut0:
    dec edx
    pushad
    push ebx 
    push printf_f
    call printf
    add esp,8
    popad
    jmp afterFirstnonZero

onlyZ:
    pushad
    push zero
    push format
    call printf
    add esp,8
    popad
endPrintOp:

%endmacro


%macro sumCopy 0
;mov dword[sum_FreePointer],0 
sumFunctionCopy:
    mov ecx,0
    mov ecx,dword[stackCounter]
    cmp ecx,2                     ; checks whether there is less than 1 operand in the stack
    jl notEnoughError             ; if so print error
    mov dword[firstEDX],0 
    mov ecx, dword[stackCounter]
    dec ecx                             ; decrease counter     
    mov ebx,0                           
    mov ebx,dword[opStack + 4*ecx]      ; puts the First operand in ebx
    mov dword[opStack + 4*ecx] ,0
    dec ecx 
    mov eax,0                    
    mov eax,dword[opStack + 4*ecx]
    mov dword[opStack + 4*ecx] ,0
    mov dword[stackCounter],ecx
.sumCopy:
    mov edx,ebx
    mov dword[HelpAddPointer],0
    push    eax               
    push    edx
    mov     ecx,0              ; counts link of the shorter operand
.cunterShorterOpCopy:
    cmp     eax,0              ; finishes when one of the operands reaches null pointer 
    je      .FirstOpIsShorterCopy
    cmp     edx,0
    je      .secondOpIsShorterCopy              ; increments link counter
    mov     eax,[eax+1]        ; go to next link of the second operand 
    mov     edx,[edx+1]        ; go to next link of the first operand 
   jmp     .cunterShorterOpCopy
.secondOpIsShorterCopy:
    pop     eax                ; in case edx is shorter- switche them
    pop     edx
    mov     dword[firstEDX],edx
    mov     dword[sumCopy_FreePointer],eax
    jmp     .add2NumbersCopy
.FirstOpIsShorterCopy:
    pop     edx                ; in case ebx is shorter- pop regular
    pop     eax
    mov dword[firstEDX],edx
      mov     dword[sumCopy_FreePointer],eax
.add2NumbersCopy:
    cmp eax, 0
    je .addNumberAndCarryCopy
    clc                        ;clear the carry flag                     ;push flag
    mov bl,[eax]
    adc [edx],bl
    mov  eax,[eax+1]                ; go to next link of the second operand 
    mov  dword[HelpAddPointer],edx   ; save pointer to the last link
    mov  edx,[edx+1]                 ; go to next link of the first operand 
    jmp .add2NumbersCopy
.addNumberAndCarryCopy:  
    cmp edx, 0
    je .checkCarry
    adc byte[edx],0
    mov  dword[HelpAddPointer],edx   ; save pointer to the last link
    mov  edx,[edx+1]                 ; go to next link of the first operand 
    jmp .addNumberAndCarryCopy    
.checkCarry:
    jc .addLinkCopy
    jmp .sumResultCopy

.addLinkCopy:
    mov eax, 0
    mov eax, 1
    add_first_Link
    mov eax,0
    mov eax,dword[HelpAddPointer]
    mov dword[pointerToLastLink+1],eax
    clc

.sumResultCopy:
    mov edx,dword[firstEDX]
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     [opStack + 4*ecx],edx ; pushes the result to the opStack 
    inc     ecx  
    mov     dword[stackCounter] ,ecx
    mov ebx,  dword[sumCopy_FreePointer]
   call freeOp           
%endmacro


%macro isEven 0
mov ecx,0
dig_num_is_even:
    push '!'
    mov dword[isEvenLen],0
    cmp edx,0
    je .end_2
.start: 
    cmp edx,0
    je .continuePop
    push edx
    inc ecx
    mov edx,[edx+1]
    jmp .start
.continuePop:
    mov eax,0
    pop eax
    cmp dword[eax],16
    jge .end_2
    mov dword[isEvenLen],1
 .end_2:
    pop eax
    cmp eax,'!'
    je .end_3
    jmp .end_2
.end_3:
%endmacro

;------------------------------------------------------------------------------------

;-------------------------print debug---------------------------------------------
print_debugg:
    mov dword[isFirstPrint],0
    mov ecx,0
    mov ecx,dword[stackCounter]
    mov eax,dword[opStack+4*ecx-4]
    mov edx,0
    mov ecx,0
    mov dword[print0_Pointer],eax
checkAll0_debugg:
    cmp eax,0
    je onlyZ_debugg
    mov cl,byte[eax]
    cmp cl,0
    jne notAll0_debugg
    mov eax,[eax+1]
    jmp checkAll0_debugg
notAll0_debugg:
    mov eax,dword[print0_Pointer]
start_print_opernd_debugg:
   cmp eax,0
   je print_print_opernd_debugg
   mov cl,byte[eax]
push_print_opernd_debugg:
    push ecx
    inc edx
    mov eax,[eax+1]
    jmp start_print_opernd_debugg
print_print_opernd_debugg:

beforeFirstnonZero_debugg:
     pop ebx
     cmp edx,0
     je endPrintOp_debugg
     cmp ebx,0
     je b2_debugg
     jmp b3
b2_debugg:
    dec edx
    jmp beforeFirstnonZero_debugg
b3_debugg:
    push ebx

afterFirstnonZero_debugg:
     pop ebx
     cmp edx,0
     je endPrintOp
cPrint_debugg:
    cmp dword[isFirstPrint],0
    je printFirst_debugg
     cmp ebx,16
     jl with0_debugg
     jmp withOut0_debugg

printFirst_debugg:
    mov dword[isFirstPrint],1
     jmp withOut0_debugg
     
with0_debugg:
    dec edx
    pushad
    push ebx 
    push format_p0
    push dword[stderr]
    call fprintf
    add esp,12
    popad
    jmp afterFirstnonZero_debugg

withOut0_debugg:
    dec edx
    pushad
    push ebx 
    push printf_f
    push dword[stderr]
    call fprintf
    add esp,12
    popad
    jmp afterFirstnonZero_debugg

onlyZ_debugg:
pushad
    push zero
    push format
    push dword[stderr]
    call fprintf
    add esp,12
popad
endPrintOp_debugg:
ret
;-------------------------end of print debug---------------------------------------------



main:                         ;---Argument handling-------------
    push    ebp
    mov     ebp,esp
    pushad
    mov     ebx,[ebp+8]   ;argc
    mov     ecx,[ebp+12]  ;argv ecx name ecx+4 var1 ecx+8 var2
    push    ecx
    push    ebx
    mov     dword[stackSize],5
    mov     byte[debug],0
    cmp     ebx,1
    je      main2
    cmp     ebx,3
    je      argc3
    mov     edx,0
    mov     edx,[ecx+4]
    cmp     word[edx],"-d"
    je      onlyD
    jmp     stackNotd
onlyD:
   mov      byte[debug],1
   jmp      main2
argc3:  
    mov     byte[debug],1

stackNotd:
    mov ecx,[ecx+4]
    movzx   ebx,byte[ecx]
    cmp     bl,'9'
    ja      notNum
    sub     ebx,'0'
    jmp     yesNum
notNum:
    sub     ebx,55

yesNum:
    inc ecx
    movzx  edx,byte[ecx]
    cmp dl,0
    je makeSize
    mov eax,ebx
    mul dword[sixteen]              ;eax num
    movzx   edx,byte[ecx]
    cmp     dl,'9'
    ja  notNumdig2
    sub     edx,'0'
    jmp     sumDig
 notNumdig2:
     sub     edx,55
 sumDig:
     add eax,edx
     mov ebx,0
     mov ebx,eax
     jmp makeSize

makeSize:
    mov     [stackSize],ebx

main2: 
    mov     eax,0
    mov     ecx,0
    mov     ebx,0
    mov     edx,0  
    mov     dword[stackCounter],0
    push    4                      ;calloc arg1
    push    dword[stackSize]                      ;calloc arg2
    call    calloc                 ;call calloc
    add     esp,8
    mov     dword[opStack],eax      ;move eax (the place assigned for thr stack) to opStack
    mov     dword[nodePtr],eax
    jmp     getCharFromUser


;-------------------------main loop--------------------------------------------------------

getCharFromUser:
    pushad
    push    calc                          
    push    format                 
    call    printf
    add     esp,8
    popad   
    push    dword [stdin]          
    push    dword 80
    push    buffer
    call    fgets                         
    add     esp, 12
    mov     eax,0
    mov     ecx,0
    mov     ebx,0
    mov     edx,0

menu:
    cmp     byte[buffer],       113       ; Check if we get quit   
    je      quitFunction
    cmp     byte[buffer],       112       ; Check if we get p (pop&print)
    je      pop_printFunction
    cmp     byte[buffer],       100       ; Check if we get d (duplicate)
    je      dupFunction
    cmp     byte[buffer],       110       ; Check if we get n (num of dig)
    je      numOfDigFunction
    cmp     byte[buffer],        38       ;  Check if we get & (and)
    je      andFunction
    cmp     byte[buffer],        124       ; Check if we get | (or)
    je      orFunction
    cmp     byte[buffer],        43       ; Check if we get + (sum)
    je      sumFunction
    
initialization:                        ;In case we finished the menu, the input is a number
cmp         byte[debug],0                      ;check if debug mod
je noD
    pushad                             ;if so,print the num
    push    buffer
    push    format
    push    dword[stderr]
    call    fprintf
    add     esp,12
    popad
noD:
    mov     ecx,dword[stackCounter]
    cmp     ecx ,dword[stackSize]          ; checks whether the stack is full
    jge     overflowError                  ; if so print error
    mov     dword [start],0                ; start- used as an buffer index
    mov     dword [digCount],0             ; digCount- used to count 2 dig for the link
    mov     dword[pointerToLastLink],0     ; pointerToLastLink- used to save the next link location
    mov     dword[isFirst],0
    mov     dword[isEvenLen],0
    mov     dword[foundFirstNum],0
    mov     dword[startAfter0],0


;--------------------INPUT NUMBER -> LIST----------------------------------------------------------------

NumToList:
    mov     dword[start],0
    mov     dword[startAfter0],0
checkZero:
    mov     edx, [start]                
    mov     bl, [buffer+edx]             ;mov to bl the new dig
    cmp     bl,0xa
    je      ok
    cmp     bl,'0'
    jne     done2
    inc     dword [start]
    inc     dword [startAfter0]
    jmp     checkZero
done2: 

checkIfEven:
    mov     edx, [start]                
    mov     bl, [buffer+edx]             ;mov to bl the new dig
    cmp     bl,0xa
    je      .doneCheckZero
    inc     dword [start]
    cmp     dword[isEvenLen],0
    je      .changeToOdd
.changeToEven:
    dec     dword[isEvenLen]
    jmp     checkIfEven
.changeToOdd:
    inc     dword[isEvenLen]
    jmp     checkIfEven
.doneCheckZero:
    mov     edx, [startAfter0]        ;put buffer index in edx
    mov     ebx,0                     ;Reset ebx for a new digit
    mov     bl, [buffer+edx]          ;put new digit in bl
    cmp     bl,0xa                    ;Checking whether we have reached the end of the Buffer
    je      end_of_num                ;if so stop the process
    sub     bl,'0'                    ;Convert bl from char to number
    cmp     bl,9                      ;chack if bl is a Letter
    jle     .mulln                    ;if bl is a number, do noting
    sub     bl,7                      ;if bl is a letter sub 7 To reach the numerical value
.mulln:
    mul     dword[sixteen]            ;Multiply the existing number by 16
    add     al,bl                     ;Add the new digit
    inc     dword[digCount]           ;Promote the dig count to know if we have enough letters to link
    cmp     dword [isFirst],0         ;See if we are on the first digit
    jne     .next                      ;if not keep moving
.first:                           ;We haven't created the first link yet
    cmp     byte[isEvenLen],0         ;chack if the buffer number is even
    je      .next       
.firstLinkOdd:                    ;Create the first link with only digit 1 because it is an odd number
    inc     byte[isFirst]             ;Notice we passed the first number
    add_first_Link                ;creat the link
    dec     dword[digCount]           ;Reset the dig because it is used for counting pairs
    mov     eax,0                     ;Reset eax for the next link

 .next:
    inc     dword [startAfter0]       ;promote the buffer index
    cmp     dword[digCount],2         ;Let's see if we have enough dig for a new link
    je      .make_node           
    jmp     .doneCheckZero           ;if not , get another dig   

.firstLink:
    inc     byte[isFirst]
    add_first_Link               ;make link
    mov     eax,0                    ;Reset eax for the next link
    dec     dword[digCount]      
    dec     dword[digCount]
    jmp     .doneCheckZero

.make_node: 
    cmp     dword [isFirst],0 
    je      .firstLink              
    add_Link                    ;make link
    mov     eax,0                   ;Reset eax for the next link
    dec     dword[digCount]      
    dec     dword[digCount]         ;Reset the dig because it is used for counting pairs
    jmp     .doneCheckZero
ok:
    mov     eax,00
    add_first_Link
end_of_num:
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    inc     ecx
    mov     dword[stackCounter],ecx
    mov     ecx,0
    jmp     getCharFromUser
    


;-----------------------------Menu Functions---------------------------------------------------------------

;-----------------------------POP AND PRINT---------------------------------------------------------------

pop_printFunction:
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx, 1                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError              ; if so print error
    mov     ebx,0
    inc dword[count]
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     ebx,0
    mov     ebx,dword[opStack+4*ecx-4] 
    mov     dword[popAndPrint_FreePointer],ebx 
    dec     ecx
    mov     dword[stackCounter],ecx
    ;mov     ecx,0  
.pop:
    mov     edx,ebx                     ; move the operand to the destination operand for printing
    print_opernd                    ; prints the number
    mov     ecx,dword[stackCounter]
    mov     ebx,0
    pushad
    mov     ebx,0
    mov ebx,dword[popAndPrint_FreePointer]
    call freeOp
    popad

    pushad                     
    push    newLine                 
    call    printf
    add     esp,4
    popad 
    jmp     getCharFromUser

;-----------------------------SUM---------------------------------------------------------------

sumFunction:
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx,2                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError             ; if so print error
    mov     ebx,0
        inc dword[count]

    mov     dword[firstEDX],0 
    mov     ecx, dword[stackCounter]
    dec     ecx                             ; decrease counter     
    mov     ebx,0                           
    mov     ebx,dword[opStack + 4*ecx]      ; puts the First operand in ebx
    ;mov     dword[opStack + 4*ecx] ,0
    dec     ecx 
    mov     eax,0                    
    mov     eax,dword[opStack + 4*ecx]
    ;mov     dword[opStack + 4*ecx] ,0
    mov     dword[stackCounter],ecx
.sum:
    mov     edx,ebx
    mov     dword[HelpAddPointer],0
    push    eax               
    push    edx
    mov     ecx,0              ; counts link of the shorter operand
.cunterShorterOp:
    cmp     eax,0              ; finishes when one of the operands reaches null pointer 
    je      .FirstOpIsShorter
    cmp     edx,0
    je      .secondOpIsShorter               ; increments link counter
    mov     eax,[eax+1]        ; go to next link of the second operand 
    mov     edx,[edx+1]        ; go to next link of the first operand 
   jmp     .cunterShorterOp
.secondOpIsShorter:
    pop     eax                ; in case edx is shorter- switche them
    pop     edx
    mov     dword[firstEDX],edx
    mov     dword[sum_FreePointer],eax
    clc
    pushf
    jmp     .add2Numbers
.FirstOpIsShorter:
    pop     edx                ; in case eax is shorter- pop regular
    pop     eax
    mov     dword[firstEDX],edx
    mov     dword[sum_FreePointer],eax
    clc 
    pushf
.add2Numbers:
    cmp     eax, 0
    je      .addNumberAndCarry            ;clear the carry flag                     ;push flag
    mov     bl,[eax]
    popf
    adc     [edx],bl
    pushf
    mov     eax,[eax+1]                ; go to next link of the second operand 
    mov     dword[HelpAddPointer],edx   ; save pointer to the last link
    mov     edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .add2Numbers

.addNumberAndCarry:  
    cmp     edx, 0
    je      .checkCarry
    popf
    adc     byte[edx],0
    pushf
    mov      dword[HelpAddPointer],edx   ; save pointer to the last link
    mov      edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .addNumberAndCarry    
.checkCarry:
    popf
    jc      .addLink
    jmp     .sumResult

.addLink:
    push     edx
    push     5
    push     1
    call    calloc 
    add     esp,8  
    pop     edx
    mov     byte[eax],1
    mov     edx,dword[HelpAddPointer]
    mov     dword[edx+1],eax
    mov     dword[eax+1],0

.sumResult:
    mov     edx,dword[firstEDX]
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     [opStack + 4*ecx],edx ; pushes the result to the opStack 
    inc     ecx  
    mov     dword[stackCounter] ,ecx       
    cmp     byte[debug],0
    pushad
    mov ebx,dword[sum_FreePointer]
    call freeOp
    popad
    je      getCharFromUser
    call    print_debugg
    jmp     getCharFromUser
   
;----------------------------- DUPLICATE---------------------------------------------------------------
dupFunction:
    mov     dword[numToCopy],0
    mov     dword[dupPointer],0
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx,1                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError             ; if so print error
    cmp     ecx ,dword[stackSize]    ; checks whether the stack is full
    jge     overflowError  
    mov     ebx,0
        inc dword[count]

    mov     edx,dword[opStack + 4*ecx-4] ;edx - num to copy
    mov     dword[numToCopy],edx
    mov     dword[isFirstDop],0
    
    dup:
        cmp edx,0
        je donePUSH
        push edx
        mov edx,[edx+1]
        jmp dup
    
     donePUSH:
        mov    ecx,0
        mov    ecx,dword[stackCounter]
        mov    edx,[opStack + 4*ecx-4]   
        mov    ebx,0 
    
    startPOP:
        cmp edx,0
        je donePOP
        pop ebx
        cmp dword[isFirstDop],0
        jne popNOT_FIRST

     popFIRST:
        mov     dword[isFirstDop],1
        push    edx
        push    ebx
        push    5
        push    1
        call    calloc
        add     esp,8
        pop     ebx
        pop     edx

        mov     cl,byte[ebx]
        mov     byte[eax], cl
        mov     dword[eax+1],0
        mov     dword[dupPointer],eax
        mov     edx,[edx+1]
        jmp     startPOP

     popNOT_FIRST:
        push    edx
        push    ebx
        push    5
        push    1
        call    calloc
        add     esp,8
        pop     ebx
        pop     edx
        mov ecx,0
        mov     cl,byte[ebx]
        mov     byte[eax], cl
        mov     ecx,dword[dupPointer]
        mov     dword[eax+1],ecx
        mov     dword[dupPointer],eax
        mov     edx,[edx+1]
        jmp     startPOP
       
     donePOP:
        mov edx,0
        mov ecx,0
        mov ecx,dword[stackCounter]
        mov edx,dword[dupPointer]
        mov dword[opStack+4*ecx],edx
        mov ebx, dword[numToCopy]
        mov dword[opStack+4*ecx-4],ebx
        
        inc ecx
        mov     dword[stackCounter],ecx
        cmp     byte[debug],0
        je      getCharFromUser
        call    print_debugg
        jmp     getCharFromUser

;----------------------------- AND---------------------------------------------------------------

andFunction:
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx,2                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError             ; if so print error
     mov     ebx,0
        inc dword[count]

    mov     dword[firstEDX],0 
    mov     ecx, dword[stackCounter]
    dec     ecx                             ; decrease counter     
    mov     ebx,0                           
    mov     ebx,dword[opStack + 4*ecx]      ;  sec num
    dec     ecx 
    mov     eax,0                    
    mov     eax,dword[opStack + 4*ecx]
    mov     dword[stackCounter],ecx
.and:
    mov     edx,ebx                     ;high in edx low eax 
    mov     dword[HelpAddPointer],0
    push    eax               
    push    edx
    mov     ecx,0                    ; counts link of the shorter operand
    
.ShorterOp:
    cmp     eax,0              ; finishes when one of the operands reaches null pointer 
    je      .FirOpIsShorter
    cmp     edx,0
    je      .secOpIsShorter               ; increments link counter
    mov     eax,[eax+1]        ; go to next link of the second operand 
    mov     edx,[edx+1]        ; go to next link of the first operand 
    jmp .ShorterOp
.secOpIsShorter:
    pop     eax                ; in case edx is shorter- switche them
    pop     edx
    mov     dword[firstEDX],edx
    mov dword[and_FreePointer],eax
    jmp     .and2Numbers
.FirOpIsShorter:
    pop     edx                ; in case ebx is shorter- pop regular
    pop     eax
    mov     dword[firstEDX],edx
    mov     dword[and_FreePointer],eax
.and2Numbers:
    cmp     eax, 0
    je      .andRestNumber
    mov     bl,[eax]
    and     [edx],bl
    mov     eax,[eax+1]                ; go to next link of the second operand 
    mov     dword[HelpAddPointer],edx   ; save pointer to the last link
    mov     edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .and2Numbers

.andRestNumber: 
    cmp     edx, 0
    je      .andResult
    and     byte[edx],0
    mov     dword[HelpAddPointer],edx   ; save pointer to the last link
    mov     edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .andRestNumber

.andResult:
    mov     edx,dword[firstEDX]
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     [opStack + 4*ecx],edx ; pushes the result to the opStack 
    inc     ecx  
    mov     dword[stackCounter] ,ecx 
    mov     dword[opStack + 4*ecx],0
    pushad
    mov ebx,dword[and_FreePointer]
    call freeOp
    popad
    cmp     byte[debug],0
    je      getCharFromUser
    call    print_debugg
    jmp     getCharFromUser    
    
;----------------------------- OR---------------------------------------------------------------

orFunction:
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx,2                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError             ; if so print error
     mov     ebx,0
        inc dword[count]

    mov     dword[firstEDX],0 
    mov     ecx, dword[stackCounter]
    dec     ecx                             ; decrease counter     
    mov     ebx,0                           
    mov     ebx,dword[opStack + 4*ecx]      ; puts the First operand in ebx
    mov     dword[opStack + 4*ecx] ,0
    dec     ecx 
    mov     eax,0                    
    mov     eax,dword[opStack + 4*ecx]
    mov     dword[opStack + 4*ecx] ,0
    mov     dword[stackCounter],ecx
.or:
    mov     edx,ebx
    mov     dword[HelpAddPointer],0
    push    eax               
    push    edx
    mov     ecx,0              ; counts link of the shorter operand
.ShorterOperand:
    cmp     eax,0              ; finishes when one of the operands reaches null pointer 
    je      .FirOperIsShorter
    cmp     edx,0
    je      .secOperIsShorter               ; increments link counter
    mov     eax,[eax+1]        ; go to next link of the second operand 
    mov     edx,[edx+1]        ; go to next link of the first operand 
    jmp .ShorterOperand
.secOperIsShorter:
    pop     eax                ; in case edx is shorter- switche them
    pop     edx
    mov     dword[firstEDX],edx
    mov     dword[or_FreePointer] ,eax
    jmp     .or2Numbers
.FirOperIsShorter:
    pop     edx                ; in case ebx is shorter- pop regular
    pop     eax
    mov     dword[firstEDX],edx
    mov     dword[or_FreePointer] ,eax
.or2Numbers:
    cmp     eax, 0
    je      .orRestNumber
    mov     bl,[eax]
    or      [edx],bl
    mov     eax,[eax+1]                ; go to next link of the second operand 
    mov     dword[HelpAddPointer],edx   ; save pointer to the last link
    mov     edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .or2Numbers
.orRestNumber:  
    cmp     edx, 0
    je      .orResult
    or      byte[edx],0
    mov     dword[HelpAddPointer],edx   ; save pointer to the last link
    mov     edx,[edx+1]                 ; go to next link of the first operand 
    jmp     .orRestNumber 
.orResult:
    mov     edx,dword[firstEDX]
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     [opStack + 4*ecx],edx ; pushes the result to the opStack 
    inc     ecx  
    mov     dword[stackCounter] ,ecx   
    pushad
    mov     ebx,dword[or_FreePointer]
    call    freeOp
    popad   
    cmp     byte[debug],0
    je      getCharFromUser
    call    print_debugg
    jmp     getCharFromUser     
    
;----------------------------- DUPLICATE---------------------------------------------------------------


;----------------------------- NUM OF DIGITS ---------------------------------------------------------------

numOfDigFunction:
    mov dword[isNodeCreated],0
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    cmp     ecx ,1                     ; checks whether there is less than 1 operand in the stack
    jl      notEnoughError             ; if so print error
    mov     ebx,0
        inc dword[count]

    mov     dword[isEvenLen],0
    mov     edx,0
    mov     ecx,0
    mov     ebx,0
    mov     ecx,dword[stackCounter]
    dec     ecx
    mov     dword[stackCounter],ecx  
    mov     edx,dword[opStack+4*ecx]       ;free memory!
    mov     dword[numOfDig_FreePointer],edx
    push    edx
    isEven  
    pop     edx

makeEmptyList:
    pushad
    push    5
    push    1
    call    calloc
    add     esp,8
    mov     ecx,0
    mov     ecx,dword[stackCounter] 
    cmp     dword[isEvenLen],0
    je      .c1
    jmp     .c2
.c1:
    mov     byte[eax],1
    jmp     .c3
.c2:
    mov     byte[eax],0
    jmp     .c3
.c3:
    mov     dword[eax+1],0
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     dword[opStack+4*ecx],eax
    inc     ecx
    mov     dword[stackCounter],ecx
    popad
    mov     dword[isFirst],0

sumNumOfDig:                    
    pushad
    cmp     edx,0
    je      DoneNumOfDig
    mov     dword[isNodeCreated],1
    push    5
    push    1
    call    calloc
    add     esp,8
    mov     ecx,0
    mov     ecx,dword[stackCounter] 
    cmp     dword[isFirst],0
    jne     .linkNotFirst
.linkfirst:
    mov     dword[isFirst],1
    mov     byte[eax],1
    jmp     .continue
.linkNotFirst:
    mov     byte[eax],2
    jmp     .continue
.continue:
    mov     dword[eax+1],0
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     dword[opStack+4*ecx],eax
    inc     ecx
    mov     dword[stackCounter],ecx
    sumCopy
    popad
    mov     edx,[edx+1]
    jmp     sumNumOfDig
DoneNumOfDig:
    cmp dword[isNodeCreated],0
    jne notonlyZero
    


onlyZero:
    push    5
    push    1
    call    calloc
    add     esp,8
    mov     ecx,0
    mov     ecx,dword[stackCounter] 
    mov     byte[eax],1
    mov     dword[eax+1],0
    mov     ecx,0
    mov     ecx,dword[stackCounter]
    mov     dword[opStack+4*ecx],eax
    inc     ecx
    mov     dword[stackCounter],ecx


notonlyZero:
    mov ebx,dword[numOfDig_FreePointer]
    call freeOp
    cmp     byte[debug],0
    je      getCharFromUser
    call    print_debugg
    jmp     getCharFromUser

;----------------------------- ERRORS---------------------------------------------------------------

 notEnoughError:           
    pushad
    push    insuf              ; in case of not Enough operands, prints an error
    push    format
    call    printf
    add     esp,8
    popad
    jmp     getCharFromUser

 overflowError:
    pushad
    push    overflow              ; in case of overflow operands, prints an error
    push    format
    call    printf
    add     esp,8
    popad
    jmp     getCharFromUser



;----------------------------- FREE MEMORY---------------------------------------------------------------

freeOp:
    
freeOp1:
    cmp         ebx,0       
    je          finishFree
    mov         edx,0
    mov         edx,[ebx+1]

    pushad
    push        ebx
    call        free
    add         esp,4            ; we push ebx as argument so we overide him when we return from function free 
    popad

    mov         ebx,0
    mov         ebx,edx       ; store the address of the next block in edx
    jmp         freeOp1
finishFree:
    ret

;----------------------------- QUIT ---------------------------------------------------------------

quitFunction:
pushad
    mov     ebx,0
    mov     ebx,dword[count]
    push    ebx
    push    printf_f
    call    printf
    add     esp,8
    popad

freestack:
    mov     ecx,dword[stackCounter]
    cmp     ecx,0
    je      emptystack
    mov     ebx,0
    pushad
    mov     ebx,[opStack+4*ecx-4]
    call    freeOp
    popad
    mov     dword [opStack+4*ecx-4],0
    dec     ecx
    mov     dword[stackCounter],ecx
    jmp freestack
emptystack:
   
lastOne:
    pushad     
    push    dword[nodePtr]
    call    free
    add     esp,4            ; we push ebx as argument so we overide him when we return from function free 
    popad 

quit2:

    mov     [ebp-4], ebx
    mov     eax, [ebp-4]
    mov     esp, ebp	; “free” function activation frame
    pop     ebp
    ret
    