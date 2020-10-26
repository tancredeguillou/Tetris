  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8


;------------------------------------------------------------------------------------


main: 
	addi sp, zero, 0x1500 ; init of the stack pointer to an empty adress


	addi s1, zero, RATE 
	call reset_game

generateDoesntOverlap: ;11111111111111111111111111111111111111111111111111111111111111111111111111111111111

movingDownPossible: ;22222222222222222222222222222222222222222222222222222222222222222222222222222222222222
	add s0 , zero, zero ; i is the sec repeat

whileRate: ;33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
	beq s1, s0, RatePasse
	call clear_leds
	call draw_gsa
	call display_score

	call wait ;wait approximately 0.2 s

	addi a0, zero, NOTHING ; 
	call draw_tetromino ; here we remove the falling tetromino from the GSA, not from the tetromino structure

	call get_input
	add a0, zero, v0 ;we get the valut of get_imput
	beq a0, zero, noAction ; if a0 =0 we don't call act
	call act
noAction:
	addi a0, zero, FALLING
	call draw_tetromino ; we redraw the falling tetromino
	addi s0, s0, 1; we incremente S0 i?i + 1 
	br whileRate ;33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333

RatePasse:
	addi a0, zero, NOTHING ; 
	call draw_tetromino ;remove the falling tetromino from the GSA, not from the tetromino structure 
	
	addi a0, zero, moveD ;try to move the falling tetromino down 
	call act
	add s2, zero, v0 ; v0 is 1 if action failed else 0
 
	addi a0, zero, FALLING ; 
	call draw_tetromino ; replace the falling tetromino by a placed tetromino
	
	beq s2, zero,movingDownPossible ; ;22222222222222222222222222222222222222222222222222222222222222222222222222222222222222

	addi a0, zero, PLACED ; 
	call draw_tetromino ; replace the falling tetromino by a placed tetromino

yechFullLine: ;while there exists a full line do 44444444444444444444444444444444444444444444444444444444444444444444444444
	call detect_full_line 
	addi t0, zero, Y_LIMIT ;
	beq v0, t0, noMoreFullLine ; if v0 = 8 there is no more full line to remove
	add a0,v0,zero ; we need TO PUT A FCKKKING ARG HERE
	call remove_full_line
	call increment_score ; increment
	call display_score ; and display the score 
	br yechFullLine ;444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
	
noMoreFullLine:
	call generate_tetromino ; generate a new tetromino 

	addi a0, zero, OVERLAP ;
	call detect_collision ; detect for overlapping collisions 

	beq a0, v0, newlyGeneratedOverlap ; if newly generated falling tetromino overlaps with something then we exit the main

	addi a0, zero, FALLING ; if no collisions then 
	call draw_tetromino ;draw the falling tetromino on the GSA 
	br generateDoesntOverlap ; and loop again ;11111111111111111111111111111111111111111111111111111111111111111111111111111111111

newlyGeneratedOverlap:
	;end so what do we have to do ?
	br main


; BEGIN: clear_leds											
															;registers : t0 / args : none
clear_leds:
	; your implementation code
	stw zero, LEDS(zero) ;assign to 0 the first 4 columns 
	addi t0, zero, 4 ; decalage for the Led(1)
	stw zero, LEDS(t0);assign to 0 the 4 columns after 
	addi t0, zero, 8; decalge for Led(2)	
	stw zero, LEDS(t0);assign to 0 the 4 columns after
	ret
; END: clear_leds


; BEGIN: set_pixel											
															;registers : t0, t1 and t2 / args : a0, a1
set_pixel:
	andi t0, a0, 0b1100 ;t0 = 4*i of Led(i)
	andi t1, a0, 0b0011 ;t1 = [0,1,2 or 3] the colomn in the given Led(i) 
	slli t1,t1,3 ;t1 = t1*8
	add t1,a1,t1;t1 = a1 + t1 : the corresponding value in Led(i) between 0 and 31
	addi t2, zero, 1; just 1
	sll t2, t2,t1; t2 = 1<<t1

	ldw t1,LEDS(t0); on load la Led concern�e (on se sert de t1 pour autre chose)
	or t1, t2, t1; on ajoute le nouveau bit aux ancien
	stw t1, LEDS(t0) ; on store la nouvelle valu 
	ret
; END: set_pixel


; BEGIN: wait
; 												registers : t0 t1
wait:
	addi t0, zero, 1 ; just one
	addi t1, zero, 20 ;replace 3 by 20 for the gecko
	sll t0,t0,t1 ; so = 1 << t1

decremente: 
	addi t0,t0,-1 ; decremente s0 de 1
	bne t0,  zero, decremente	
	ret
; END: wait


; BEGIN: in_gsa												
															;registers : t0,t1,t2,t3,t4,t5 / args : a0,a1 
in_gsa:														;return v0 = 1 if out of GSA, 0 if in GSA
	; your implementation code
	cmplti t0, a0, 0 ; (a0 < 0) ? 1 : 0
	cmpgei t1, a0, X_LIMIT ; (a0 >= 12) ? 1 : 0
	or t2, t0, t1 ; a0 is outside the gsa if t0 is 1 OR t1 is 1
	cmplti t0, a1, 0 ; (a1 < 0) ? 1 : 0
	cmpgei t1, a1, Y_LIMIT ; (a1 >= 8) ? 1 : 0
	or t3, t0, t1 ; a1 is outside the gsa if t0 is 1 OR t1 is 1

	or v0, t2, t3 ; v0 = t2 or t3 : if one of the coordinates is outside, the entire location is outside

	ret 
; END: in_gsa


; BEGIN: get_gsa													
																;registers : t1,t2 / args : a0,a1
get_gsa:														;return v0 = Element at location (x,y) in the GSA
	; your implementation code
	slli t1, a0, 3 ; we want to find the position of (x, y), which is : x * 8 + y
	add t2, t1, a1

	slli t1, t2, 2 ; starting at GSA, the adress of the word is t2 * 4
	ldw v0, GSA(t1) ; the word at the given adress directly gives the state 
	ret 
; END: get_gsa


; BEGIN: set_gsa													
															;registers : t0,t1,t2 / args : a0,a1,a2
set_gsa:
	; your implementation code
	slli t1, a0, 3 ; we want to set the value at position (x, y), which is : x * 8 + y
	add t2, t1, a1

	slli t1, t2, 2 ; the adress of the word is t2 * 4
	stw a2, GSA(t1) ; the word at the given adress directly gives the state 
	ret
; END: set_gsa


; BEGIN: draw_gsa											 	registers : t3, t4, t5 are constants, a0 & a1 needed to call get_gsa,
draw_gsa:													; 	v0 & v1 to retrieve a0 & a1 original values at the end / args : none
	; your implementation code									calls get_gsa & set_pixel which use registers t0, t1, t2
	addi sp, sp, -24
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw ra, 8(sp); we store the value of the return adress 
	stw s3, 12(sp); we store the value of s3
	stw s4, 16(sp); we store the value of s4
	stw s5, 20(sp); we store the value of s5
;======================================================
	call clear_leds ; we start by clearing all leds

	add a0, zero, zero ; position of x, starting at 0
	add a1, zero, zero ; position of y, starting at 0
	addi s3, zero, NOTHING ; we must generate a constant NOTHING
	addi s4, zero, 7 ; we will iterate on y, it can't go beyond 7
	addi s5, zero, 11 	; constant 11
loop1:
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call get_gsa ; we get the gsa value of led at position (x, y)
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================

	beq v0, s3, loop2 ; if the value is nothing we skip the setting part

	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call set_pixel
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
loop2:
	addi a1, a1, 1 ; we increment y by 1
	bge s4, a1, loop3 ; if 7 >= y we have not gone beyond and we enter the loop3
	add a1, zero, zero ; ELSE, y goes back to being 0
	addi a0, a0, 1 ; and x is incremented by 1
loop3:
	bge s5, a0, loop1 	; if t5 (11) >= x, then we can continue the loop, i.e go back to loop1
;======================================================
	ldw a0, 0(sp); we load the value of a0
	ldw a1, 4(sp); we load the value of a1
	ldw ra, 8(sp); we load the value of the return adress 
	ldw s3, 12(sp); we store the value of s3
	ldw s4, 16(sp); we store the value of s4
	ldw s5, 20(sp); we store the value of s5
	addi sp, sp, 24
;======================================================
	ret
; END: draw_gsa


; BEGIN: draw_tetromino 									 registers : t0,t1, s0,s1/ args : a0 (GSA value p) 
draw_tetromino:
	addi sp, sp, -24
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw ra, 8(sp); we store the value of the return adress
	stw a2, 12(sp); we store the value of a2
	stw s0, 16(sp)
	stw s1, 20(sp)
;======================================================


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldw t0, T_type(zero)  ; t0 = type(0=C,1=B,2=T...)
	ldw t1, T_orientation(zero); t1 = orientation(0=N,1=E,2=So,3=W)

	; INDEX STEP IS 4
	slli t0, t0, 4 ; we multiply t0 by 16 == to <<4
	slli t1, t1, 2 ; we multiply t1 by 4 == to <<2
	add t0, t0 ,t1 ; t0 = index of the corresponding offset in the DRAW_Ax and DRAW_Ay

	;we use t4 t5 instead of t1 t2 because set_gsa use them
 	ldw s0, DRAW_Ax(t0);address of offset in x
	ldw s1, DRAW_Ay(t0);address of offset in y

	add a2, zero, a0

	ldw a0, T_X(zero) ;current Xpos of anchor-----------------------------------------------------------0
 	ldw a1, T_Y(zero) ;current Ypos of anchor

	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call set_gsa ; set the anchor ----------------------------------------------------------------------0
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================


 	ldw t0, 0(s0) ;offest in x--------------------------------------------------------------------------1
	ldw t1, 0(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add a0, t2, t0
	add a1, t3, t1
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call set_gsa ; set the anchor ----------------------------------------------------------------------1
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================

 	ldw t0, 4(s0) ;offest in x--------------------------------------------------------------------------2
	ldw t1, 4(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add a0, t2, t0
	add a1, t3, t1
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call set_gsa ; set the anchor ----------------------------------------------------------------------2
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	
 	ldw t0, 8(s0) ;offest in x--------------------------------------------------------------------------3
	ldw t1, 8(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add a0, t2, t0
	add a1, t3, t1
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call set_gsa ; set the anchor ----------------------------------------------------------------------3
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;======================================================

	ldw a0, 0(sp); we get back the value of a0
	ldw a1, 4(sp); we get backt he value of a1
	ldw ra, 8(sp); we get back the value of the return adress 
	ldw a2, 12(sp); we store the value of a2
	ldw s0, 16(sp)
	ldw s1, 20(sp)
	addi sp, sp, 24
;======================================================
	ret
; END: draw_tetromino 


; BEGIN: generate_tetromino									registers : / args : none
generate_tetromino:
	; your implementation code
	addi t2, zero, 4 ; we will use this value to test our random number
loopGenerate:						
	;stw t0, RANDOM_NUM(zero) ; this generates a random number ???
	;ldw t1, RANDOM_NUM(zero) ; we retrieve the random number
	;and t1, t1, t0 ; we take only the last three bits of the random number
	;blt t2, t1, loop ; if 4 < random_num than we re compute a number
	ldw t1, RANDOM_NUM(zero)
	andi t1, t1, 0b0111  ; we take only the last three bits of the random number
	blt t2, t1, loopGenerate ; if 4 < random_num than we re compute a number

;-------------------------------basic op-------------------------------------------------------
	addi t0, zero, START_X ; we can use the previous registers as we don't need them anymore
	stw t0, T_X(zero)
	addi t0, zero, START_Y
	stw t0, T_Y(zero)
	addi t0, zero, N ; orientation : North
	stw t0, T_orientation(zero)
;----------------------------------------------------------------------------------------------
	stw t1, T_type(zero) ; type : the value of the random number
	
;======================================================
	ret
; END: generate_tetromino


; BEGIN: detect_collision											registers : / args : a0
detect_collision:
	; your implementation code
	addi sp,sp, -32
	stw a0, 0(sp) ; start by saving value of a0
	stw a1, 4(sp) ; start by saving value of a1
	stw ra, 8(sp) ; as we are going to call a few processes, we keep track of the oringinal return adress
	stw s3, 12(sp);---------------------------------switch t0 by s0
	stw s4, 16(sp);---------------------------------switch t4 by s4
	stw s5, 20(sp);---------------------------------switch t5 by s5
	stw s0, 24(sp)
	stw s1, 28(sp)

	addi s3, zero, PLACED ; we initialize a constant Placed

west:
	addi t2, zero, W_COL
	bne a0, t2, est ; if s0 isn't 0, then we are note in the W_COL case
	addi s4, zero, -1 ; if we are, then each x must be decremented and tested
	addi s5, zero, 0 ; y doesn't change as we are just moving to the left
	jmpi test_col
est:
	addi t2, zero, E_COL
	bne a0, t2, south ; if s0 isn't 1, then we are not in the E_COL case
	addi s4, zero, 1 ; if we are, then each x must be incremented and tested
	addi s5, zero, 0 ; y doesn't change as we are just moving to the right
	jmpi test_col
south:
	addi t2, zero, So_COL
	bne a0, t2, ovlap
	addi s4, zero, 0 ; in the south case, x doesn't change
	addi s5, zero, 1 ; but y does, it's incremented
	jmpi test_col
ovlap:
	add s4, zero, zero ; if we are in the overlap case, then we don't change the current values of the points,
	add s5, zero, zero ; as we need to see if a current overlap exists !
test_col:	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldw t0, T_type(zero)  ; t0 = type(0=C,1=B,2=T...)
	ldw t1, T_orientation(zero); t1 = orientation(0=N,1=E,2=So,3=W)

	; INDEX STEP IS 4
	slli t0, t0, 4 ; we multiply t0 by 16 == to <<4
	slli t1, t1, 2 ; we multiply t1 by 4 == to <<2
	add t0, t0 ,t1 ; t0 = index of the corresponding offset in the DRAW_Ax and DRAW_Ay

	;we use t4 t5 instead of t1 t2 because set_gsa use them
 	ldw s0, DRAW_Ax(t0);address of offset in x
	ldw s1, DRAW_Ay(t0);address of offset in y

	ldw t1, T_X(zero) ; now we will test the gsa of each point that will be occupied if we proceed to a move----------------0
	add a0, t1, s4 ; each x coordinate is in/decremented depending on what we decided
	ldw t1, T_Y(zero)
	add a1, t1, s5 ; same thing for each y coordinate

	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call in_gsa ; now we call in_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================

	bne v0, zero, col ; if the new point is out of bounds, then we can immediately end and tell there is a collision
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call get_gsa ; now we call get_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================

	beq v0, s3, col ; if the new point is already PLACED, then we can immediately end and tell there is a collision---------0

 	ldw t0, 0(s0) ;offest in x----------------------------------------------------------------------------------------------1
	ldw t1, 0(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add t0, t2, t0
	add t1, t3, t1

	add a0, t0, s4
	add a1, t1, s5
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call in_gsa ; now we call in_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	bne v0, zero, col ; if the new point is out of bounds, then we can immediately end and tell there is a collision
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call get_gsa ; now we call get_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	beq v0, s3, col ; if the new point is already PLACED, then we can immediately end and tell there is a collision---------1

 	ldw t0, 4(s0) ;offest in x----------------------------------------------------------------------------------------------2
	ldw t1, 4(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add t0, t2, t0
	add t1, t3, t1

	add a0, t0, s4
	add a1, t1, s5
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call in_gsa ; now we call in_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	bne v0, zero, col ; if the new point is out of bounds, then we can immediately end and tell there is a collision
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call get_gsa ; now we call get_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	beq v0, s3, col ; if the new point is already PLACED, then we can immediately end and tell there is a collision---------2
	
 	ldw t0, 8(s0) ;offest in x----------------------------------------------------------------------------------------------3
	ldw t1, 8(s1);offset in y
	ldw t2, T_X(zero)
	ldw t3, T_Y(zero)

	add t0, t2, t0
	add t1, t3, t1

	add a0, t0, s4
	add a1, t1, s5
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call in_gsa ; now we call in_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	bne v0, zero, col ; if the new point is out of bounds, then we can immediately end and tell there is a collision
	;=====================================
	addi sp, sp, -16
	stw a0, 0(sp); we store the value of a0
	stw a1, 4(sp); we store the value of a1
	stw a2, 8(sp)
	;=====================================
	call get_gsa ; now we call get_gsa for this new point
	;=====================================
	ldw a0, 0(sp); we store the value of a0
	ldw a1, 4(sp); we store the value of a1
	ldw a2, 8(sp)
	addi sp, sp, 16
	;=====================================
	beq v0, s3, col ; if the new point is already PLACED, then we can immediately end and tell there is a collision---------3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	addi v0, zero, NONE ; if no collision was found, then v0 is put to NONE, and we end the process
	br no_col
col:
	ldw v0, 0(sp) ; a collision was found, v0 takes the value of the original a0
no_col:
	ldw a0, 0(sp)
	ldw a1, 4(sp)
	ldw ra, 8(sp)
	ldw s3, 12(sp)
	ldw s4, 16(sp)
	ldw s5, 20(sp)
	ldw s0, 24(sp)
	ldw s1, 28(sp)
	addi sp,sp, 32
	ret 
; END: detect_collision


; BEGIN: rotate_tetromino								registers : t0, t1 \arg a0: Rotation direction
rotate_tetromino:		
	addi t0, zero, rotL ; 2 is the value of rotL
	beq  a0, t0, rotateLeft 	; if equal 2 rotateLeft 
	addi t0, zero, 1 ; t0 = 1	else rotateRight (rotR)
	jmpi rotate
						
rotateLeft:
	addi t0, zero, -1 ;t0 = -1

rotate:
	ldw t1, T_orientation(zero) ; t1 = Mem[T_orientation]
	add t1, t1, t0	; t1 = t1 + t0
	andi t1, t1, 0b0011 ; t1 equals the 2 last digit of itself
	stw t1, T_orientation(zero)
	ret
; END: rotate_tetromino

; BEGIN: act											
													;registers \arg a0: value of the action
													;return v0: 0 if succeeded, 1 if failed
act:	
	addi sp,sp,-12 ; we save a0 because if we rotate we can change it
	stw a0, 0(sp)
	stw s0, 4(sp) ; we store the initial value of the  orientation in case we need to delete the changements
	stw s1, 8(sp); we also store the init x component for the same reason

	ldw s0, T_orientation(zero) ; we load the initial value of the  orientation in case we need to delete the changements
	ldw s1, T_X(zero); we also load the init x component for the same reason

	addi t0, zero, moveD ; 0x20 is the value of moveD
	beq  a0, t0, moveDown 	; if equal moveDown
 											
	addi t0, zero,moveR ;0x10 is the value of moveR
	beq  a0, t0, moveRight 	; if equal moveRight

	addi t0, zero, moveL ;0x01 is the value of moveL
	beq  a0, t0, moveLeft	; if equal moveLeft

	addi t0, zero,rotL ;0x02 is the value of rotL
	beq  a0, t0, rotations 	; if equal rotateTetromino

	addi t0, zero,rotR ;0x08 is the value of rotR
	beq  a0, t0, rotations 	; if equal rotateTetromino	

	addi t0, zero,reset ;0x04 is the value of reset
	beq  a0, t0, resetTheGame 	; if equal resetTheGame	
 

moveDown:
	addi a0, zero, So_COL ; we check the south
	addi sp,sp,-8; we save ra because we are gonna call 
	stw ra, 0(sp)
	stw a0, 4(sp)
	call detect_collision ;call and not br because it's out of the scope of act
	ldw a0, 4(sp)
	ldw ra, 0(sp);we reload the value of ra
	addi sp,sp,8
	beq v0, a0, failedAct ; if v0 = a0 there is a collision 
	ldw t0, T_Y(zero)
	addi t0, t0, 1 ;we add 1 to T_Y
	stw t0, T_Y(zero)
	br successfulAct
	
moveRight:
	addi a0, zero, E_COL ; we check the est
	addi sp,sp,-8; we save ra because we are gonna call 
	stw ra, 0(sp)
	stw a0, 4(sp)
	call detect_collision ;call and not br because it's out of the scope of act
	ldw a0, 4(sp)
	ldw ra, 0(sp);we reload the value of ra
	addi sp,sp,8
	beq v0, a0, failedAct ; if v0 = a0 there is a collision 
	ldw t0, T_X(zero)
	addi t0, t0, 1; we add 1 to T_X
	stw t0, T_X(zero)
	br successfulAct

moveLeft:
	addi a0, zero, W_COL ; we check the ouest
	addi sp,sp,-8; we save ra because we are gonna call 
	stw ra, 0(sp)
	stw a0, 4(sp)
	call detect_collision ;call and not br because it's out of the scope of act
	ldw a0, 4(sp)
	ldw ra, 0(sp);we reload the value of ra
	addi sp,sp,8
	beq v0, a0, failedAct ; if v0 = a0 there is a collision 
	ldw t0, T_X(zero)
	addi t0, t0, -1; we subtract 1 to T_X
	stw t0, T_X(zero)
	br successfulAct
	
rotations:
	addi sp,sp,-4; we save ra because we are gonna call 
	stw ra, 0(sp);we store ra
	;stw a0, 4(sp);we also store a0 because we call detect_collision with another arg
	call rotate_tetromino ; first we rotate

	addi a0, zero, OVERLAP ; we check overlap
	call detect_collision ;then we check if there is an overlap
	ldw ra, 0(sp);we reload the value of ra
	;ldw a0, 4(sp)
	addi sp,sp,4
	addi t0, zero, OVERLAP ; not sur if i can use t0
	bne v0, t0, successfulAct ; if v0 != OVERLAP the we are happy

;-----------------------------------------------------------------------------------
;----now we have an overlap, so we try to move the tetromino toward the center ----
;-----------------------------------------------------------------------------------

	addi s3, zero, 2 ; counter to the number of pixels we can add to the anchor point

tryAvoidOverlap: 
	beq s3, zero, failedAct
	addi s3, s3, -1

	addi t2, zero, 6 ; constant which correspond to the middle column
	ldw t0, T_X(zero)
	blt s1, t2, tryRight ; if T_X < 6 we need to move to the right (S1 is saved !!!!)
	addi t1, t0, -1 ;else to  the left
;	addi t0, zero, moveR
	br check

tryRight: 
	addi t1, t0, 1
;	addi t0, zero, moveL

check:
	stw t1, T_X(zero)
	;stw t0, T_orientation(zero)
;---------------------------------------------------------------------------------
;	call update_tetromino I BELIEVE NOW THAT WE DON'T USE ADRESSES WE UPDATE DIRECTLY IN COLLISION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	addi a0, zero, OVERLAP ; we check if ther is still an overlaap

	call detect_collision ;call and not br because it's out of the scope of act

;---------------------------------------------------------------------------------

	addi a0, zero, OVERLAP ; we reset a0 a OVERLAP
	bne v0, a0, successfulAct ; if v0 and a0 are different there is no overlap so we can stay like this
	br tryAvoidOverlap ; else try again to shift (if possible)

resetTheGame:
	addi sp,sp,-4; we save ra because we are gonna call 
	stw ra, 0(sp)
	call reset_game ;call and not br because it's out of the scope of act
	ldw ra, 0(sp);we reload the value of ra
	addi sp,sp,4

	ldw a0, 0(sp)
	ldw s0, 4(sp) ; we reload the initial value of the  orientation because we need to delete the changements
	ldw s1, 8(sp); we reload store the init x component for the same reason
	addi sp, sp, 12
	addi v0, zero, 1
	ret

failedAct:
	stw s0, T_orientation(zero) ; we restore the initial value of the  orientation because the action failed
	stw s1, T_X(zero); we also restore the init x component for the same reason

	ldw a0, 0(sp)
	ldw s0, 4(sp) ; we reload the initial value of the  orientation because we need to delete the changements
	ldw s1, 8(sp) ; we reload store the init x component for the same reason
	addi sp, sp, 12
	addi v0, zero, 1
	ret
successfulAct:
	ldw a0, 0(sp)	
	ldw s0, 4(sp) ; we store the initial value of the  orientation because we need to delete the changements
	ldw s1, 8(sp) ; we also store the init x component for the same reason
	addi sp, sp, 12
	add v0, zero, zero
	ret
; END: act

; BEGIN:get_input													registers :  / args : none
get_input:													
	; your implementation code
	addi t0, zero, 4
	ldw t0, BUTTONS(t0)
	add v0, zero, zero
	addi t1, zero, 1 ; initialize a mask
	addi t3, zero, 32 ; we stop at the fourth bit, i.e 16
loop_get:
	beq t1, t3, end_get
	and t4, t0, t1
	bne t4, zero, one ; if the given bit of edgecapture is 1, stop the loop
	slli t1, t1, 1 ; else shift the mask
	br loop_get
one:
	add v0, zero, t1 ; as t1 is shifted accordingly it will always be the wanted value
end_get:
	addi t0, zero, 4
	stw zero, BUTTONS(t0)

	ret
; END:get_input


; BEGIN:detect_full_line
detect_full_line:	
									; return v0: y-coordinate of the full line closest to the top of the game screen or 8, if there are no full lines.
	addi sp, sp, -20
	stw a0, 0(sp) ; we save a0 and a1 because we don't know if they were used before
	stw a1, 4(sp)
	stw ra, 8(sp)
	stw s0, 12(sp)
	stw s1, 16(sp)

	add s0, zero, zero ; the index of the line we check y
	add s1, zero, zero ; index for the column x

tryNextPos:
	addi t2, zero, X_LIMIT ; 12
	beq s1, t2, exit ; if s1 = 12 that means all the previous x of this line are on
	add a0, zero, s1
	add a1, zero, s0
	call get_gsa ; we saved ra at the beginnin of the method
	addi t4, zero, PLACED ; t4 = 1 
	addi s1, s1, 1 ; we incremente the x pos t3
	beq v0,t4, tryNextPos ; if v0 = 1 : if the led(t3,t1) is on then we try the next x on this line 

beginNextLine:
	addi t1, zero, Y_LIMIT ; 8
	beq s0, t1, exit ; if t0 = 8 we return 8
	addi s0, s0, 1 ; we incremente the index of the line
	add s1, zero, zero ; index for the column x reset to 0
	br tryNextPos  

exit:
	add v0, zero, s0

	ldw a0, 0(sp) ; we re load a0 and a1 because we don't know if they were used before
	ldw a1, 4(sp)
	ldw ra, 8(sp) ; we get back the ra
	ldw s0, 12(sp)
	ldw s1, 16(sp)
	addi sp, sp, 20

	ret
; END:detect_full_line


; BEGIN: remove_full_line
 remove_full_line:															; register a0: y-coordinate of the full line to be removed
	addi sp, sp, -32
	stw a0, 0(sp) ; we store a0 and a1 because we don't know if they were used before
	stw a1, 4(sp)
	stw ra, 8(sp) ; we wanna get back the ra
	stw a2, 12(sp)
	stw s1, 16(sp); we wanna keep the value of s1
	stw s2, 20(sp); we wanna keep the value of s2
	stw s3, 24(sp); we wanna keep the value of s3
	stw s4, 28(sp); we wanna keep the value of s4

	add s2, zero, a0 ; s2 take the y-coordinate of the full line to be removed
	add s4, zero, zero; s4 is the number of time we are gonna pass through beginSwitch
	addi s3, zero, 1 ; arg we are gonna give to set_gsa... 1 at the beginning (to be xor) 

beginSwitch:
	addi t1, zero, 5 ; off + on + off + on + off = 5 changements (de 0 a 4)
	beq s4, t1, removeNextLines ; if s4=5 then we end the blink and remove the line
	addi s4, s4, 1 ; we passed here one more time
	add s1, zero, zero ; init index for the column x=0
	xori s3, s3, 0x0001 ; we invert the last bit so 0 then 1 then 0 then 1 then 0

switchLine:
	addi t0, zero, X_LIMIT ; 12
	beq s1, t0, draWait ; x equal 12 : we finished the line
	add a0, zero, s1 ; a0 is the x coordinate
	add a1, zero, s2 ; a1 is the y coordinate
	add a2, zero, s3 ; a2 is the p value we put inthe gsa 	
	call set_gsa ; we update the gsa
	addi s1, s1, 1 ; we incremente s1
	br switchLine

draWait:
	call draw_gsa
	call wait
	br beginSwitch

removeNextLines: 
	add s4, s2, zero ; we init the counter to s2  which will be our y var

removeNextLine:
	add s1, zero, zero ; init index for the column x=0
	addi s4, s4, -1 ; we decremente s4 so we begin at the index of the line removed -1
	addi t1, zero, -1
	beq s4,t1, deleteFirstLine ; if we arrive at -1, we have updated all the line excet the first one which is unchanged

moveOthersDown:
	addi t0, zero, X_LIMIT ; 12
	beq s1, t0, removeNextLine ; x equal 12 : we finished the line
	add a0, zero, s1 ; a0=s1 is the x coordinate
	add a1, zero, s4 ; a1=s4 is the y not s2 anymore because now y move
	call get_gsa ; now v0 has the value of g(s1,s4)
	add a0, zero, s1 ; a0=s1 is the x coordinate
	addi a1, s4, 1 ; a1=s4+1
	add a2, zero, v0 ; a2=v0 is the p value we put in the gsa 	
	call set_gsa ; we update the gsa g(s1,s4+1) = g(s1,s4) : we just moved down the value of the cell
	addi s1, s1, 1 ; we incremente s1
	br moveOthersDown ; we do the same for all the x of the line

deleteFirstLine:
	addi t0, zero, X_LIMIT ; 12
	beq s1, t0, endRemove ; x equal 12 : we finished the first line so finally end
	add a0, zero, s1 ; a0=s1 is the x coordinate
	add a1, zero, zero ; a1=zero is the y concerned
	addi a2, zero, NOTHING ; a2=NOTHING=zero is the p value we put in the gsa 	
	call set_gsa ; we update the gsa g(s1,zero) = NOTHING
	addi s1, s1, 1 ; we incremente s1
	br deleteFirstLine ; we do the same for all the x of the first line

endRemove:

	ldw a0, 0(sp) ; we re load a0 and a1 because we don't know if they were used before
	ldw a1, 4(sp)
	ldw ra, 8(sp) ; we get back the ra
	ldw a2, 12(sp)
	ldw s1, 16(sp) ; we wanna keep the value of s1
	ldw s2, 20(sp) ; we wanna keep the value of s2
	ldw s3, 24(sp) ; we wanna keep the value of s3
	ldw s4, 28(sp) ; we wanna keep the value of s4
	addi sp, sp, 32
	ret
; END:  remove_full_line


; BEGIN:increment_score													registers : t0, t1 / args : none
increment_score:													
	; your implementation code
	ldw t0, SCORE(zero) ; load the score
	addi t1, zero, 9999
	beq t0, t1, end_inc ; if the score = 9999, do nothing as it is maximum score
	addi t0, t0, 1 ; else we increment t0
	stw t0, SCORE(zero) ; and store the new score in score adress
end_inc:
	ret
; END:increment_score


; BEGIN:display_score													registers : / args : none
display_score:													
	; your implementation code
	ldw t0, SCORE(zero) ; load the score
	add t2, zero, zero ; the value we will use for the LED thousands display index
	add t3, zero, zero ; the value we will use for the LED hundreds display index
	add t4, zero, zero ; the value we will use for the LED tens display index
	add t5, zero, zero ; the value we will use for the LED units display index
thousands:
	addi t1, zero, 1000 ; we want to check how many thousands we have
	blt t0, t1, hundreds ; if score is less than 1000 we can go to the tens
	addi t0, t0, -1000 ; else we substract 1000 from the score
	addi t2, t2, 1 ; and we add 1 to the index
	br thousands ; and we loop
hundreds:

	addi t1, zero, 100 ; we want to check how many hundreds we have
	blt t0, t1, tens ; if score is less than 100 we can go to the tens
	addi t0, t0, -100 ; else we substract 100 from the score
	addi t3, t3, 1 ; and we add 1 to the index
	br hundreds ; and we loop
tens:

	addi t1, zero, 10 ; we want to check how many tens we have
	blt t0, t1, units ; if score is less than 10 we can go to the tens
	addi t0, t0, -10 ; else we substract 10 from the score
	addi t4, t4, 1 ; and we add 1 to the index
	br tens ; and we loop
units:

	beq t0, zero, display ; if the score is zero we can display 
	addi t0, t0, -1 ; else we substract 1 from the score
	addi t5, t5, 1 ; and we add 1 to the index
	br units ; and we loop
display:
	add t0, zero, zero ; value we will use for SEVEN_SEGS adresses
	; display the thousands
	slli t2, t2, 2 ; t2 = t2*4
	ldw t2, font_data(t2) ; load the needed font data
	stw t2, SEVEN_SEGS(t0) ; and store it in the seven_segs adress
	addi t0, t0, 4 ; increment t0 by 4
	; display the hundreds
	slli t3, t3, 2 ; t3 = t3*4
	ldw t3, font_data(t3) ; load the needed font data
	stw t3, SEVEN_SEGS(t0) ; and store it in the seven_segs adress
	addi t0, t0, 4 ; increment t0 by 4
	; display the tens
	slli t4, t4, 2 ; t4 = t4*4
	ldw t4, font_data(t4) ; load the needed font data
	stw t4, SEVEN_SEGS(t0) ; and store it in the seven_segs adress
	addi t0, t0, 4 ; increment t0 by 4
	; display the units
	slli t5, t5, 2 ; t5 = t5*4
	ldw t5, font_data(t5) ; load the needed font data
	stw t5, SEVEN_SEGS(t0) ; and store it in the seven_segs adress
	ret 
; END:display_score


; BEGIN:reset_game
reset_game:
	; your implementation code

	addi sp,sp,-8
	stw ra, 0(sp)
	stw a0, 4(sp)
	stw zero, SCORE(zero) ; game score is zero
	call display_score
	addi t1, zero, 96 ; there are 96 GSA adresses, from 0 to 95
	slli t1, t1, 2 ; adress indexes so 96*4
	add t0,zero,zero
gsa_reset_loop:
	stw zero, GSA(t0) ; we store zero at the GSA adress
	addi t0, t0, 4 ; we increment t0 by 4
	bne t0, t1, gsa_reset_loop ; while t0 != t1 we loop, to restart all adresses

	call generate_tetromino ; we generate a new tetromino
	addi a0, zero, FALLING
	call draw_tetromino
	call clear_leds
	call draw_gsa ; the leds are lit accordingly to the game state array
	
	ldw ra, 0(sp)
	ldw a0, 4(sp)
	addi sp,sp,8
	ret
; END: reset_game


;--------------------------------------------------------------------------------------------

font_data:
    .word 0xFC  ; 0
    .word 0x60  ; 1
    .word 0xDA  ; 2
    .word 0xF2  ; 3
    .word 0x66  ; 4
    .word 0xB6  ; 5
    .word 0xBE  ; 6
    .word 0xE0  ; 7
    .word 0xFE  ; 8
    .word 0xF6  ; 9

C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
    .word C_N_X ;0
    .word C_E_X ;4
    .word C_So_X ;8
    .word C_W_X ;12
    .word B_N_X ;16
    .word B_E_X ; ...
    .word B_So_X
    .word B_W_X
    .word T_N_X
    .word T_E_X
    .word T_So_X
    .word T_W_X
    .word S_N_X
    .word S_E_X
    .word S_So_X
    .word S_W_X
    .word L_N_X
    .word L_E_X
    .word L_So_X
    .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
    .word C_N_Y
    .word C_E_Y
    .word C_So_Y
    .word C_W_Y
    .word B_N_Y
    .word B_E_Y
    .word B_So_Y
    .word B_W_Y
    .word T_N_Y
    .word T_E_Y
    .word T_So_Y
    .word T_W_Y
    .word S_N_Y
    .word S_E_Y
    .word S_So_Y
    .word S_W_Y
    .word L_N_Y
    .word L_E_Y
    .word L_So_Y
    .word L_W_Y
