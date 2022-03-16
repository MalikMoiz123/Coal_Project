INCLUDE Irvine32.inc
print_game proto, arr:ptr byte
fill_with_random proto ,arr:ptr byte
null_occ proto,bomb_num:byte
fill_with_X proto , arr:ptr byte
fill_with_random3 proto , arr:ptr byte
num_to_str proto,num:dword,str_num:ptr dword

;level2
print_game2 proto ,arr:ptr byte
fill_with_random2 proto , arr:ptr byte



.data
;-------------Guiding strings---------;
cr_str db "----------------CRUSH ROW-------------",0
cc_str db "----------------CRUSH COL-------------",0

;level_3 db "--------------Level 3 started--------------",0
;level_2 db "--------------Level 2 Started--------------",0
level_1 db "--------------Level 1 Started--------------",0

rand_str db "----------------PERC UP----------------",0
fill_rand db "--------------RANDOM NUMBER--------------",0
bombx db "---------------BOMB--------------------",0


;------------------FILE HANDLING-----------------------;

filename byte "data.txt",0
filehandle dword ?
buffer byte 30 dup(?)
file_cr byte "File is created ",0
file_cr1 byte "File is not created",0
file_l1 byte " : LEVEL 1 : ",0
file_l2 byte " : LEVEL 2 : ",0
file_l3 byte " : LEVEL 3 : ",0
noofbytes dword ?
file_score dword 10 dup("0")
;-----------------END OF FILE HANDLING------------------;
;-------------Initialization------;
game_arr db 10 dup(0)
         db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)
		 db 10 dup(0)

row db 10
col db 10

;-------End Initialization-------;

;-------name-score-moves--------;
namex db 30 dup("0")
score dword 0
moves dword 0
name_string db "NAME : ",0
score_string db "SCORE : ",0
moves_string db "MOVES : ",0
info_space db"        ",0
;-------------------------------;

;-------print_game---------;
disp_dots db "....",0
disp_sep db " | ",0
disp_dot_space db"     ",0
col_disp db 1
col_space db "   ",0
col_space_sep db"     ",0

row_disp db 1
row_space db "   |",0
p_out db 0
p_in db 0
;------End Print Game-------;



;------Input function Starts-----;
row1 db 0
col1 db 0
row2 db 0
col2 db 0 
block1_str db "SELECT YOUR BOX 1",0
row1_str db "Enter Row(1) : ",0
col1_str db "Enter Col(1) : ",0

block2_str db "SELECT BOX TO SWAP",0
row2_str db "Enter Row(2) : ",0
col2_str db "Enter Col(2) : ",0
;--conditonsblock2--;
row_up db 0
row_down db 0
col_up db 0
col_down db 0

invalid_input db "You've Chosen an Invalid Box",0

enter_name db "Enter Your Name : ",0 
swap_str db "SWAPPED SUCCESS",0
;------Input function Ends-------;

;-----swap function-----------;

;-----perculateup--------;



str1 byte "-----------Swap not possible-----------",0
score_count dword 0
;------Crush Rows------------; 
i_r dword ?                                                              ;initial row index                
i_c dword ?                                                              ;initial col index
e_r dword ?
e_c dword ?
r_refrence_variable db ?                                              ;refrence variable for rows
r_current_variable db ?                                               ;refrence variable for current variable
r_seq_count dword ?                                                      ;sequence count
cr_i dword ?
cr_j dword ?


crush_check db 0                                                    ;if crush check==0 then new  turn otherwise repopulate and crush row col

;---------Crush Cols----------------------;
;i_r dword ?                                                              ;initial row index                
;i_c dword ?                                                              ;initial col index
;e_r dword ?
;e_c dword ?
c_refrence_variable db ?                                              ;refrence variable for rows
c_current_variable db ?                                               ;refrence variable for current variable
c_seq_count dword ?                                                      ;sequence count
cc_i dword ?
cc_j dword ?




.code
main PROC



  
   call driver_level1

	call WaitMsg
 exit 
main ENDP




;----------------RANDON NUMBER FILLER-------------;
fill_with_random proc , arr:ptr byte
pushad
mov esi,arr                                                   ;moves the address of array into esi
mov ecx,10                                                    ;sets ecx to 10
mov ebx,0                                                     ;EBX follows Rows - set to 0
mov edi,0                                                     ;EDI follows Cols - set to 0
random1:
push ecx                                                      ;Stores Loop counter for loop1 in stack

mov ecx,10
random2:
cmp game_arr[ebx+edi],0                                       ;if this particular index is empty
je empty_state                                                ;than jump to the empty state label

back_to_random2:                                              ;BACK_TO_RANDOM2 LABEL connects with empty_state label
inc edi                                                       ;desired operations done, incremenets to next col
loop random2                                                  ;END OF RANDOM2 LOOP
mov edi,0                                                     ;sets cols to 0 again
add ebx,10                                                    ;move to next row 
pop ecx                                                       ;restores random1 counter
loop random1                                                  ;END OF RANDOM1 LOOP

jmp to_end                                                    ;after loop out of scope jmps to end ignoring emptystate label
empty_state:                                                  ;EMPTY STATE LABEL
mov eax,0
mov eax,6                                                     ;sets range for random numnber generation
call randomrange                                              ;produces random number
inc al
mov game_arr[ebx+edi],al                                           ;asssign random number to the particular arr index

jmp back_to_random2                       

to_end:


popad
ret
fill_with_random endp
;------------END OF RANDOM NUMBER FILLER-----------;

;--------------------NULL-OCCURENECE---------------;
;     CONV 6 into bomb in the array                ;
;--------------------------------------------------;
null_occ proc,bomb_num:byte
pushad


mov esi,0                                          ;esi refresh
mov edi,0                                          ;edi refresh
mov ecx,100                                        ;loop indexes

l1:                                                ;loop l1
mov eax,0                                          ;refresh eax
mov al,bomb_num                                    ;moves num we want to destroy in al
cmp game_arr[esi],al                               ;game_arr comparison with al
je d_c                                             ;comes back to the loop
back:                                              ;BACK LOOP
inc esi                                            ;increments esi
loop l1                                            ;dec l1

jmp to_end   
d_c:
mov game_arr[esi],0
inc score
jmp back
to_end:

popad
ret
null_occ endp
;---------------END NULL OCCURENCE------------------;



;----------------------PRINT GAME------------------;
print_game proc ,arr:ptr byte
pushad
;-------prints name---------;
lea edx,name_string 
call writestring
lea edx,namex
call writestring

lea edx,info_space
call writestring
;-------prints score-------;
lea edx,score_string
call writestring
mov eax,score
call writedec
lea edx,info_space
call writestring
;-----prints moves---------;
lea edx,moves_string
call writestring
mov eax,moves
call writedec

call crlf 
call crlf
call crlf


mov esi, arr                                                  ;passes address of game_arr into esi
mov ecx,10                                                    ;sets print loop 1 counter to 10
mov ebx,0                                                     ;row access register, COL=0
mov edi,0                                                     ;col access register, Row=0


;----------Prints Starting few spaces-------;
lea edx,col_space_sep                                         ;space print before col index disp (staring few spaces)
call writestring
mov al," "
call writechar                                                ;COL_SPACE_SEP PRINTED

;--------Prints Cols Indexes---------------;
col_ind_disp:                                                 ;LOOP COL_INDEX_DISP START
mov al,col_disp
call writedec

lea edx,col_space
call writestring
inc col_disp
loop col_ind_disp                                            ;LOOP COL_INDX_DISP END

mov col_disp,1                                               ;COL_DISP (prints cols indexes) reset to 1
call crlf
call crlf
;------END Print Col Indexes---------------;

;---------------First Loop Starts------------------;
mov ecx,10                                                    ;Sets Loop for Counter
print1:
push ecx                                                      ;saves PRINT1 counter

mov ecx,10                                                    ;prints dots counter set to 10





lea edx,disp_dot_space                                       ;Prints spaces before printing dot sperator
call writestring

;----------------Prints Dots As Seperator---------;
p_dot1:                                                       ;LABEL PDOT1
lea edx,disp_dots                                             ;displays the dots address passed 
call writestring                                              ;prints dots "...."
loop p_dot1                                                   ;END LABEL PDOT1

call crlf                                                     ;moves to next line
mov al," "
call writechar
;-----------------prints Row Number------------------;
mov al,row_disp
call writedec
lea edx,row_space
call writestring
inc row_disp

;--------------Second loop Starts--------------------;
mov ecx,10                                                    ;sets counter for print2

print2:                                                       ;START PRINT2 LOOP

mov al,game_arr[ebx+edi]                                     ;moves content at particular game index to al
cmp al,'X'
je print_char
cmp al,6
je print_bomb
cmp al,'o'
je print_space

call writedec                                                 ;prints the array
back:
lea edx,disp_sep                                              ;passes spaces into 
call writestring                                              ;prints " | "
inc edi                                                       ;inc cols
loop print2                                                   ;END PRINT2 LOOP
;------------End Second Loop-------------------------;
call crlf
mov edi,0                                                     ;resets the col to 0
add ebx,10                                                    ;moves to next row
pop ecx                                                       ;restores loop counter of print1

loop print1                                                    ;END PRINT1 LOOP
;----------End First Loop-----------------------------;


;--------------prints last dot line-------------------;
lea edx,disp_dot_space
call writestring
mov ecx,10
;-------Label p_dot2 starts-----------------;
p_dot2:                                                       ;LABEL PDOT2
lea edx,disp_dots                                             ;displays the dots address passed 
call writestring                                              ;prints dots "...."
loop p_dot2                                                   ;LABEL PDOT2
call crlf
call crlf
call crlf
;------label p_do2 ends--------------------;

mov row_disp,1                                                ;ROW_DISP (display row Index ) RESET TO 1
popad
jmp exit1
print_char:
call writechar
jmp back
print_bomb:
mov eax,0
mov al,'B'
call writechar
jmp back
print_space:
mov eax,0
mov al,' '
call writechar
jmp back


exit1:
ret
print_game endp
;------------END OF PRINT GAME---------------------; 

;--------------------INPUT FUNCTION----------------;
input_func proc 
pushad

wrong_input1:
;----prints box1 string----;
lea edx,block1_str
call writestring
call crlf
call crlf

;----input in row 1------;
lea edx,row1_str     
call writestring

mov eax,0                                                ;reset eax 
call readdec                                             ;reads a integer value
mov row1,al                                              ;moves readed value into row1

;----input in col1------;
lea edx,col1_str
call writestring

mov eax,0                                                ;movs eax to 0                              
call readdec                                             ;reads  a decimal
mov col1,al                                              ;sets col1 to value readed

cmp row1,10
ja wrong_input_message1
cmp row1,1
jb wrong_input_message1
cmp col1,10
ja wrong_input_message1
cmp col1,1
jb wrong_input_message1



jmp to_end_input                                          ;transfer control to end of the loop

wrong_input_message1:                                      ;invalid input condtion
lea edx,invalid_input
call writestring
call crlf
jmp wrong_input1                                           ;prints message and jmps back to loop


to_end_input:                                            ;end of input function

;-------test------;
wrong_input2:
;----prints box2 string----;
lea edx,block2_str                                        ;prints block2 string 
call writestring
call crlf
call crlf

;----input in row 2------;
lea edx,row2_str     
call writestring

mov eax,0                                                ;reset eax 
call readdec                                             ;reads a integer value
mov row2,al                                              ;moves readed value into row2

;----input in col2------;
lea edx,col2_str
call writestring

mov eax,0                                                ;movs eax to 0                              
call readdec                                             ;reads  a decimal
mov col2,al                                              ;sets col2 to value readed


cmp row2,10
ja wrong_input_message2
cmp row2,1
jb wrong_input_message2
cmp col2,10
ja wrong_input_message2
cmp col2,1
jb wrong_input_message2

;other condtions check

;------------CASE1 - UPSAME----------;
mov bl,10

mov al,row1
dec al
mul bl
mov cl,al
sub cl,10                                              ;al becomes the index value 1 row upper 

;mov row_up,al                                           ;assign row up value stored in al
mov al,row2
dec al
mul bl

cmp al,cl                                              ;comparing upper row of box1 and row2
je up_same                                              ;if row2 of box2 and upper row of box equal
col_up_ne:
;----------END CASE1 - UPSAME--------;

;---------CASE2 - DOWN SAME---------;
mov bl,10

mov al,row1
dec al
mul bl
mov cl,al
add cl,10

mov al,row2
dec al
mul bl

;mov row_down al
cmp al,cl
je down_same
col_down_ne:
;---------END CASE2 -  DOWNSAME--------;

;------CASE3 - RIGHT SAME-----------;
mov al,col1
inc al
cmp col2,al
je right_same
row_right_ne:
;-----------END CASE3 -RIGHT SAME-------;

;---------CASE 4 - LEFT SAME-----------;
mov al,col1
dec al
cmp col2,al
je left_same
row_left_ne:
;----------END CASE 4 LEFT SAME-----------;
jmp wrong_input_message2

;else to the end
jmp to_end2

wrong_input_message2:
call crlf
lea edx,invalid_input
call writestring
call crlf
jmp wrong_input2

;-----------------UPSAME------------;
up_same:
mov al,col1                                             ;moves col1 into al
cmp col2,al                                             ;compares them since both must be same in this case
je call_swap
jmp col_up_ne

jmp to_end2 
;---------------DOWNSAME----------;
down_same:
mov al,col1
cmp col2,al
je call_swap
jmp col_down_ne
;-------------Right Same----------;
right_same:
mov al,row1
cmp row2,al
je call_swap
jmp row_right_ne
;------------Left Same------------;
left_same:
mov al,row1
cmp row2,al
je call_swap
jmp row_left_ne
;--------------------------------;


call_swap:
;call the swap function by passing the parameters
call swap_func
;lea edx,swap_str
;call writestring
call crlf

to_end2:

inc moves
call waitmsg
call crlf
invoke print_game, addr game_arr

popad
ret
input_func endp
;-----------------END INPUT FUNCTION---------------;

;--------------SWAP FUNCTION-----------------------;
swap_func proc
pushad
mov dl,10

mov al,row1                                   ;decides row
dec al                                        ;decremenets al value
mul dl                                        ;multiplies al and dl and stores result in al

movzx esi,al                                  ;ROW1
movzx edi,col1                                ;COL1
dec edi

mov al,row2
dec al
mul dl

movzx ebx,al                                  ;ROW2
movzx ebp,col2                                ;COL2
dec ebp

mov al,game_arr[ebx+ebp]
cmp al,'X'
je swap_not
cmp al,6
je bomb_found_1
cmp al,'o'
je swap_not


cmp game_arr[esi+edi],'X'
je swap_not
cmp game_arr[esi+edi],6
je bomb_found_2


cmp game_arr[esi+edi],'o'
je swap_not

xchg game_arr[esi+edi],al
mov game_arr[ebx+ebp],al
call clrscr
lea edx,swap_str
call writestring
call crlf
jmp to_end

swap_not:
lea edx,str1
call writestring
call crlf
call crlf
call input_func
jmp to_end

end1:

bomb_found_1:
mov game_arr[ebx+ebp],0
mov al,game_arr[esi+edi]
invoke null_occ,al 
lea edx,bombx
call writestring
jmp to_end

bomb_found_2:
mov game_arr[esi+edi],0
mov al,game_arr[ebx+ebp]
invoke null_occ,al 
lea edx,bombx
call writestring
jmp to_end


to_end:
;call waitmsg

;call clrscr

;invoke print_game, addr game_arr
popad
ret
swap_func endp 
;---------END OF SWAP FUNCTION---------------------;

;--------------CRUSH ROWS --------------------------;
crush_rows proc 
pushad
mov cr_i,10
mov cr_j,9

mov esi,0                                                          ;esi 0 (source indexing)
mov edi,0                                                          ;edi 0 (direct indexing)


m_outer:                                                           ;MAIN OUTER LOOP LABEL (STARTS)
cmp cr_i,0                                                         ;check if outerloop counter==0 mov to MAIN_OUTERLOOP_END
je m_outer_end

mov cr_j,9                                                         ;sets counter for inner loop
m_inner:                                                           ;MAIN INNER LOOP LABEL (STARTS)
cmp cr_j,0                                                         ;check if innterloop counter==0 mov to MAIN_INNERLOOP_EN
je m_inner_end

;logical Code here
 
 
mov al,game_arr[esi+edi]                                           ;AL serves as Reference Variable 
cmp al,'X'
je end_1
cmp al,6
je end_1
cmp al,'o'
je end_1

mov bl,game_arr[esi+edi+1]                                         ;BL serves as Current Varuabile



cmp al,bl                                                          ;compares elseif Ref_variable==Current_variable
je loop_to_seq_inc                                                 ;if both are equal increment in Sequence counter                       


cmp al,bl                                                          ;IF Refrence!=CurrentVariable
jne loop_to_crush_seq                                              ;Jumps from loop to Crush Sequence

crush_seq_to_loop:                                                 ;From CrushSequence tag back to Loop

seq_inc_to_loop:                                                   ;Comes back from Sequence_Increment tag to loop
mov edx,r_seq_count                                                
cmp edx,2
je save_initials                                                  ;checks if seq_count>=2 and saves initials

save_i_to_loop:
end_1:
inc esi                                                            ;traverses all rows one by one

;--test
cmp esi,9
je check_seq 

last_back:
;--test

dec cr_j                                                           ;Decremenets inner loop counter
jmp m_inner                                                        ;JMPS TO MAIN INNER LOOP (ENDS)
m_inner_end:                                                       ;MAIN_INNER_END 

mov esi,0                                                          ;Cols traversal (always starts from 0)
add edi,10                                                         ;Traverse Rows by addition of 10 each time
mov r_seq_count,0

dec cr_i                                                          ;Decremenets outer loop counter
jmp m_outer                                                       ;JMPS TO MAIN OUTER LOOP (ENDS)

jmp m_outer_end                                                   ;--------JUMPS TO END OF PROGRAM--------;

loop_to_seq_inc:                                                  ;if reference variable==current variable (inc seq count)
inc r_seq_count
jmp seq_inc_to_loop                                               


loop_to_crush_seq:
mov edx,r_seq_count                                                 ;moves seq_count into dl
cmp edx,2
jae crush_seq
mov r_seq_count,0
jmp crush_seq_to_loop

;---test
check_seq:
cmp r_seq_count,2
jae crush_seq
jmp last_back
;--test

crush_seq:
;crush_check++
inc crush_check

;e_r = i;
mov edx,i_r
mov e_r,edx 

;e_c = i_c + seq_count;
mov edx,i_c
add edx,r_seq_count
mov e_c,edx

;setting up outer loop
mov ebx,i_r
mov ebp,i_c

;for (int b = i_c; b <= e_c; b++)
mov edx,e_c
sub edx,i_c
inc edx

mov ecx,edx
crush_outer:
mov game_arr[ebx+ebp],0
inc ebp
loop crush_outer
mov edx,r_seq_count    ;updates score
inc edx
add score,edx    ;update score

mov r_seq_count,0      ;update seq count
jmp crush_seq_to_loop

save_initials:
mov edx,edi                    ;esi keeps track of the cols
mov i_r,edx

mov edx,esi
dec edx
mov i_c,edx
jmp save_i_to_loop



m_outer_end:                                                      ;MAIN_INNER_END


popad


invoke print_game, addr game_arr
;lea edx,cr_str
;call writestring
;call crlf
;CALL CRLF
;end_1:
;popad
ret
crush_rows endp
;-----------END OF CRUSH ROWS----------------------;


;-------------------------CRUSH COLS----------------;
crush_cols proc 
pushad
mov cc_i,10
mov cc_j,9

mov esi,0                                                          ;esi 0 (source indexing)
mov edi,0                                                          ;edi 0 (direct indexing)


m_outer:                                                           ;MAIN OUTER LOOP LABEL (STARTS)
cmp cc_i,0                                                         ;check if outerloop counter==0 mov to MAIN_OUTERLOOP_END
je m_outer_end

mov cc_j,9                                                         ;sets counter for inner loop
m_inner:                                                           ;MAIN INNER LOOP LABEL (STARTS)
cmp cc_j,0                                                         ;check if innterloop counter==0 mov to MAIN_INNERLOOP_EN
je m_inner_end

;logical Code here
 
 
mov al,game_arr[esi+edi]                                           ;AL serves as Reference Variable 
cmp al,'X'
je end_2
cmp al,6
je end_2
cmp al,'o'
je end_2


mov bl,game_arr[esi+edi+10]                                         ;BL serves as Current Varuabile

cmp al,bl                                                          ;compares elseif Ref_variable==Current_variable
je loop_to_seq_inc                                                 ;if both are equal increment in Sequence counter                       


cmp al,bl                                                          ;IF Refrence!=CurrentVariable
jne loop_to_crush_seq                                              ;Jumps from loop to Crush Sequence

crush_seq_to_loop:                                                 ;From CrushSequence tag back to Loop

seq_inc_to_loop:                                                   ;Comes back from Sequence_Increment tag to loop
mov edx,c_seq_count                                                
cmp edx,2
je save_initials                                                  ;checks if seq_count>=2 and saves initials

save_i_to_loop:
end_2:
add esi,10                                                         ;traverses all rows one by one

cmp esi,90
je check_seq

last_back:

dec cc_j                                                           ;Decremenets inner loop counter
jmp m_inner                                                        ;JMPS TO MAIN INNER LOOP (ENDS)
m_inner_end:                                                       ;MAIN_INNER_END 

mov esi,0                                                          ;Cols traversal (always starts from 0)
inc edi                                                            ;Traverse Rows by addition of 10 each time
mov c_seq_count,0

dec cc_i                                                          ;Decremenets outer loop counter
jmp m_outer                                                       ;JMPS TO MAIN OUTER LOOP (ENDS)

jmp m_outer_end                                                   ;--------JUMPS TO END OF PROGRAM--------;

loop_to_seq_inc:                                                  ;if reference variable==current variable (inc seq count)
inc c_seq_count
jmp seq_inc_to_loop                                               


loop_to_crush_seq:
mov edx,c_seq_count                                                 ;moves seq_count into dl
cmp edx,2
jae crush_seq
mov c_seq_count,0
jmp crush_seq_to_loop

check_seq:
cmp c_seq_count,2
jae crush_seq 
jmp last_back 


crush_seq:
;crush_check++
inc crush_check



;setting up outer loop
mov ebx,i_r
mov ebp,i_c

;for (int b = i_r; b <= e_r; b++)

mov edx,c_seq_count
inc edx
mov ecx,edx

crush_outer:
mov game_arr[ebx+ebp],0
add ebp,10
loop crush_outer


mov edx,c_seq_count    ;updates score
inc edx
add score_count,edx    ;update score
mov c_seq_count,0      ;update seq count

jmp crush_seq_to_loop

save_initials:
mov edx,esi
sub edx,10
mov i_r,edx

mov edx,edi                 
mov i_c,edx
jmp save_i_to_loop



m_outer_end:                                                      ;MAIN_INNER_END

mov edx,score_count
add score,edx
mov score_count,0

invoke print_game, addr game_arr
;lea edx,cc_str
;call writestring
;CALL CRLF 
;CALL CRLF
popad
ret
crush_cols endp
;--------------END OF CRUSH COLS--------------------;

;-----------------Perculate UP----------------------;
perculate_up proc

pushad
mov esi,0    ;cols
mov edi,0    ;rows

mov ecx,10
p_up_outer:
push ecx
mov ecx,10
p_up_inner:

mov al,game_arr[esi+edi]
cmp game_arr[esi+edi],0
je to_is_empty

from_empty_to_loop:
inc esi
loop p_up_inner

pop ecx
add edi,10
mov esi,0
loop p_up_outer
jmp to_end

to_is_empty:
cmp edi,0
jne perc_up
jmp from_empty_to_loop


perc_up:
mov ebx,edi           ;moves row to ebx
mov ebp,esi           ;moves cols to ebp

mov eax,ebx           ;transfers ebx value to eax
mov dl,10
div dl               ;divides eax by 10


push ecx
mov ecx,eax           ;moves divisor into ecx

moveup:

cmp game_arr[ebx+ebp-10],'o'
je o_here

mov al,game_arr[ebx+ebp]
mov dl,game_arr[ebx+ebp-10]


mov game_arr[ebx+ebp],dl
mov game_arr[ebx+ebp-10],al

sub ebx,10
loop moveup

o_here:
pop ecx
jmp from_empty_to_loop

to_end:



invoke print_game, addr game_arr
;lea edx,rand_str
;call writestring
;CALL CRLF
;CALL CRLF

popad
ret
perculate_up endp
;------------------END PERCULATE  UP-----------------;


;----------------------------------------------------------;
;                NUM TO STRING FILE HANDLING               ;
;----------------------------------------------------------;
;num_to_str proc,num:dword,str_num:ptr dword
;pushad
;mov eax,0
;mov eax,num
;cmp eax,0
;je zero
;mov bl,10
;mov esi,str_num
;add esi,3
;LC1:
;div bl
;add ah, 30H
;mov [esi],ah
;dec esi
;mov ah,0
;cmp eax,10
;jl done
;loop LC1
;zero:
;mov bl,'0'
;mov [esi],bl
;jmp return

;done:
;add eax,30h
;mov [esi],eax

;return:
;popad
;ret
;num_to_str endp
;----------------------------------------------------;



;------------------FILE SCORE------------------------;
file_scores proc
pushad

;-----level 1---------;
lea edx,file_l1
mov eax,filehandle
mov ecx,lengthof file_l1
call writetofile




lea edx,score_count
mov eax,filehandle
call writetofile




mov eax,filehandle
call closefile


popad
ret
file_scores endp

;-----------------END OF FILE SCORE------------------;





;-----------------DRIVER LEVEL 1---------------------;
driver_level1 proc
pushad
mov moves,0


lea edx,enter_name
call writestring

mov eax,0 
lea edx,namex
mov ecx,lengthof namex
call readstring 
mov ebx,eax

call waitmsg
call clrscr

lea edx,filename
call createoutputfile
mov filehandle,eax

lea edx,name_string
mov eax,filehandle
mov ecx,lengthof name_string
call writetofile


lea edx,namex
mov ecx,ebx
mov eax,filehandle
call writetofile


call file_scores

 lea edx,level_1
 call writestring
 call  crlf

main_driver:
    call crlf
    invoke fill_with_random,addr game_arr
    invoke print_game, addr game_arr
	call input_func
driver:

lea edx,cr_str
call writestring
call crlf

call crush_rows
call waitmsg
call crlf


lea edx,cc_str
call writestring
call crlf
call crush_cols
call waitmsg
call crlf
	

lea edx,rand_str
call writestring
call crlf
call perculate_up
call waitmsg
call crlf


lea edx,fill_rand
call writestring
CALL CRLF 
invoke fill_with_random,addr game_arr
invoke print_game, addr game_arr
	
call waitmsg
call crlf
cmp crush_check,0
je to_driver

mov crush_check,0
jmp driver

to_driver:
cmp moves,2                                           ;:----------ADJUST MOVES HERE-------------:;
jae to_end

jmp main_driver
to_end:

popad
ret
driver_level1 endp
;----------------DRIVER LEVEL 1 ENDS-----------------;


;-----------------------------------------------MAIN DRIVER 2----------------------------------------------------;
;                                             LEVEL 2 CODE BEGINS                                                ;
;----------------------------------------------------------------------------------------------------------------;


;------------MAKING BOARD---------------------;
level2_board PROC
pushad
mov esi,0
mov edi,0

;-----------------REFRESHING THE ARRAY------------

mov ecx,10                             
l2_l1:
push ecx
mov ecx,10
l2_l2:
mov game_arr[esi+edi],0                
inc esi
loop l2_l2
mov esi,0
add edi,10
pop ecx
loop l2_l1


;-----------initializing first 3 rows and columns to obtain first quarter of (+) sign-----------
mov edi , 0
mov ebx , 0
mov ecx , 0
mov ecx , 3


L1:
push ecx
mov ecx , 3
L2:
mov game_arr[ebx+edi] ,'o'
inc edi
loop L2

pop ecx
add ebx , 10
mov edi , 0
loop L1

;----------------initializing bottom left to obtain (+) sign---------------------------------

mov ebx , 0
mov edi , 0
mov ebx , 70
mov edi , 0
mov ecx , 3

put1:
push ecx
mov ecx , 3

put2:
mov game_arr[ebx+edi] , 'o'
inc edi

loop put2

pop ecx
add ebx , 10
mov edi , 0

loop put1


;----------------initializing top right to obtain (+) sign---------------------------


mov ebx , 0
mov edi , 7
mov ecx , 3
put3:
push ecx
mov ecx , 3
put4:
mov game_arr[ebx+edi] , 'o'
inc edi
loop put4
pop ecx
add ebx , 10
mov edi , 7
loop put3


;----------------initializing bottom right to obtain (+) sign------------------------

mov ebx , 70
mov edi , 7
mov ecx , 3
put5:
push ecx
 mov ecx , 3
 put6:
 mov game_arr[ebx+edi] , 'o'
 inc edi
 loop put6
 pop ecx
 add ebx , 10
 mov edi , 7
 loop put5

 ;----------initializing the middle part first-------------------------;
mov ebx,40           ;row 
mov edi,3            ;col

mov ecx,2
null_mid1:
push ecx
mov ecx,4

null_mid2:
mov game_arr[ebx+edi],'o'
inc edi
loop null_mid2
add ebx,10
mov edi,3

pop ecx
loop null_mid1

popad
ret
level2_board endp


;----------------------------DRIVER LEVEL2--------------------;

driver_level2 proc
pushad                                                             ;saves reg values
mov moves,0

;------------INITIALZING THE BOARD------------;
call level2_board 
;
; lea edx,level_2
 call writestring
 call  crlf

;------------------------------------------------;

main_driver3:
    call crlf                                                       ;endl
    invoke fill_with_random2,addr game_arr                          ;populated array
    invoke print_game, addr game_arr                                ;prints the populated array
	call input_func                                                 ;asks user for the input

driver3:
lea edx,cr_str                                                      ;Crush col string
call writestring                                                    ; prints crushing string
call crlf
	call crush_rows                                                 ;Crushes the row
	call waitmsg                                                    ;halts the screen
	call crlf                                                       ;ENDL


lea edx,cc_str                                                      ;Crush col string
call writestring                                                    ; prints crushing string
call crlf
	call crush_cols                                                 ;crushes cols
	call crlf
	call waitmsg
	call crlf

	

lea edx,rand_str                                                   
call writestring
call crlf
	call perculate_up                                               ;Perculates up after crushing
	call crlf
	call waitmsg
	call crlf

lea edx,fill_rand
call writestring
call crlf
	invoke fill_with_random2,addr game_arr                          ;after crushing all random seqs
	invoke print_game, addr game_arr                                ;prints game
	call waitmsg
	call crlf

	cmp crush_check,0
	je to_driver3

	mov crush_check,0
jmp driver3

to_driver3:
cmp moves,2                                       ;:----------ADJUST MOVES HERE-------------:;
jae to_end

jmp main_driver3
to_end:

popad
ret
driver_level2 endp


;-------------END DRIVER LEVEL 2-----------------;




;-------------START RANDOM FILLER------------------;


fill_with_random2 proc , arr:ptr byte
pushad

mov esi,arr                                                   ;moves the address of array into esi
mov ecx,10                                                    ;sets ecx to 10
mov ebx,0                                                     ;EBX follows Rows - set to 0
mov edi,0                                                     ;EDI follows Cols - set to 0
random1:
push ecx                                                      ;Stores Loop counter for loop1 in stack

mov ecx,10
random2:
cmp game_arr[ebx+edi],0                                       ;if this particular index is empty
je empty_state                                                ;than jump to the empty state label

back_to_random2:                                              ;BACK_TO_RANDOM2 LABEL connects with empty_state label
inc edi                                                       ;desired operations done, incremenets to next col
loop random2                                                  ;END OF RANDOM2 LOOP
mov edi,0                                                     ;sets cols to 0 again
add ebx,10                                                    ;move to next row 
pop ecx                                                       ;restores random1 counter
loop random1                                                  ;END OF RANDOM1 LOOP

jmp to_end                                                    ;after loop out of scope jmps to end ignoring emptystate label
empty_state:                                                  ;EMPTY STATE LABEL
mov eax,5                                                     ;sets range for random numnber generation
call randomrange                                              ;produces random number
add al,1

cmp al,5
je bomb 
backbomb:
mov game_arr[ebx+edi],al                                           ;asssign random number to the particular arr index

jmp back_to_random2                       
bomb:
inc al
jmp backbomb


to_end:


popad
ret
fill_with_random2 endp
;------------END OF RANDOM NUMBER FILLER-----------;




;----------ADDITIONAL RESOURCE FOR RANDOM NUMBER 2------------;
sub_one proc
pushad
mov esi,0
mov ecx,100

l1:
cmp game_arr[esi],'o'
je back
cmp game_arr[esi],6
jne subtract
back:
inc esi
loop l1

jmp to_end
subtract:
dec game_arr[esi+edi]
jmp back

to_end:
popad
ret
sub_one endp
;-------------END OF SUB-ONE----------------------------------;


;-----------END OF FUNCTIONS LEVEL 2-----------------------------;
;                                                                ;
;----------------------------------------------------------------;



;-----------------------------------------------MAIN DRIVER 3---------------------------------------------------;
;                                             LEVEL 3 CODE BEGINS                                               ;
;---------------------------------------------------------------------------------------------------------------;



;-------------DRIVER LEVEL 3---------------------;
;    Special Driver Function for Level 3         ;
;------------------------------------------------;

;-------------END DRIVER LEVEL 3--------;



;-----------FILL WITH RANDOM X---------------;
;        Replaces 6 in the array with X      ;
;--------------------------------------------;
fill_with_X proc , arr:ptr byte

mov esi,arr                                                   ;moves the address of array into esi
mov ecx,10                                                    ;sets ecx to 10
mov ebx,0                                                     ;EBX follows Rows - set to 0
mov edi,0                                                     ;EDI follows Cols - set to 0
rand1:
push ecx                                                      ;Stores Loop counter for loop1 in stack

mov ecx,10
rand2:
cmp game_arr[ebx+edi],6                                       ;if this particular index is = 6
je empty_state                                                ;than jump to the empty state label

back_to_rand2:                                              ;BACK_TO_RANDOM2 LABEL connects with empty_state label
inc edi                                                       ;desired operations done, incremenets to next col
loop rand2                                                  ;END OF RANDOM2 LOOP
mov edi,0                                                     ;sets cols to 0 again
add ebx,10                                                    ;move to next row 
pop ecx                                                       ;restores random1 counter
loop rand1                                                  ;END OF RANDOM1 LOOP

jmp to_end1                                                    ;after loop out of scope jmps to end ignoring emptystate label
empty_state:                                                  ;EMPTY STATE LABEL
mov eax,0
mov game_arr[ebx+edi],'X'                                          ;asssign X to the particular arr index
                                                                   ;because index is of 1-5
jmp back_to_rand2                       

to_end1:
ret
fill_with_X endp
;-------------END FILL WITH RANDOM X---------;



;----------------RANDON NUMBER FILLER of level 3-------------;
; Random Num generator in level 3 - RANGE : (1-6) - 
;uses fill_with_x to replace 6 wtih "X" char
;------------------------------------------------------------;
;fill_with_random3 proc , arr:ptr byte

;mov esi,arr                                                   ;moves the address of array into esi
;mov ecx,10                                                    ;sets ecx to 10
;mov ebx,0                                                     ;EBX follows Rows - set to 0
;mov edi,0                                                     ;EDI follows Cols - set to 0
;random3:
;push ecx                                                      ;Stores Loop counter for loop1 in stack

;mov ecx,10
;random31:
;cmp game_arr[ebx+edi],0                                      ;if this particular index is empty
;je empty_state                                                ;than jump to the empty state label

;back_to_random3:                                              ;BACK_TO_RANDOM2 LABEL connects with empty_state label
;inc edi                                                       ;desired operations done, incremenets to next col
;loop random31                                                  ;END OF RANDOM2 LOOP
;mov edi,0                                                     ;sets cols to 0 again
;add ebx,10                                                    ;move to next row 
;pop ecx                                                       ;restores random1 counter
;loop random3                                                  ;END OF RANDOM1 LOOPs
;fill_with_random3 endp
END main