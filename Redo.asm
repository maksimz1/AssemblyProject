IDEAL
MODEL small
STACK 100h

macro PUSH_ALL
	push ax
	push bx
	push cx
	push dx
	push di
	push si
endm

macro POP_ALL
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
endm

PLAYER_LENGTH = 5
PLAYER_SPEED = 1
ENEMY_SPEED = 1
MOVE_DELAY = 200
macro PUSH_ALL_BP
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si
endm

macro POP_ALL_BP
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
endm

DATASEG

grid db (320/PLAYER_LENGTH)*(200/PLAYER_LENGTH) dup(0) ; 64 * 40

player_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Ah)
trail_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Bh)
enemy_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Ch)

black_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup (0)
matrix dw ?

playerX dw 0 ; Player's Location relative to GRID
playerY dw 0
playerDirection db 0 ; Directions of movement: 0 - up, 1 - right, 2 - down, 3 - left

Enemy1X dw 0
Enemy1Y dw 0
Enemy1Direction db 0

Enemy2X dw 0
Enemy2Y dw 0
Enemy2Direction db 0

Enemy3X dw 0
Enemy3Y dw 0
Enemy3Direction db 0

Enemy4X dw 0
Enemy4Y dw 0
Enemy4Direction db 0

isExit db 0
RndCurrentPos dw 0
timer dw 0
second_counter dw 0

CODESEG
start:
	mov ax, @data
	mov ds, ax
	mov ax, 13h
	int 10h
	call PlayGame
	
exit:
	mov ax, 4c00h
	int 21h

; in dx how many cols 
; in cx how many rows
; in matrix - the offset of what to copy
; in di start byte in screen (where to paste)

proc putMatrixInScreen
    push es
    push ax
    push si

    mov ax, 0A000h
    mov es, ax
    cld ; for movsb playerDirection si --> di
	
    mov si,[matrix]
@@NextRow:
    push cx

    mov cx, dx
    rep movsb ; Copy whole line to the screen, si and di advances in movsb
    sub di,dx ; returns back to the begining of the line 
    add di, 320 ; go down one line by adding 320

    pop cx
    loop @@NextRow

    pop si
    pop ax
    pop es
    ret
endp putMatrixInScreen

; push the X
; push the Y
; result in stack
; Description: Function that calculates the needed Offset in video segment using the given coordinates

proc CoordinatesToVideo
	PUSH_ALL_BP
	mov bx, 320*PLAYER_LENGTH
	mov ax, [bp+4]
	xor dx, dx
	mul bx
	xor dx, dx
	push ax
	mov ax, [bp+6]
	mov bx, PLAYER_LENGTH
	mul bx
	pop bx
	add ax, bx
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp

; Main Function and Loop of Game	

proc PlayGame
	
	call CreateEnemy1
	; call CreateEnemy2
	; call CreateEnemy3
	; call CreateEnemy4
	
	@@InputLoop:
	push 1 
	call LoopDelay ; Makes a delay for one milisecond
	call ChangeDirection ; Takes input and changes direction accordingly
	
	; Makes the delay for updating and drawing the game Objects
	mov ax, [timer] 
	mov bx, MOVE_DELAY
	xor dx, dx
	div bx
	cmp dx, 0
	jne @@cont ; If 200 Miliseconds haven't passed, keep counting
	
	mov ax, [timer] 
	mov bx, 1000
	xor dx, dx
	div bx
	cmp dx, 0
	jne @@PrintTime
	mov [second_counter], ax
	@@PrintTime:
	push 0
	push 0
	call MoveCursor
	mov ax, 1000
	push [second_counter] 
	push 0ah
	call PrintNumWithColor
	
	call ErasePlayer ; Erase the outdated position of the Player
	call UpdatePlayer ; Updates Player's position according to playerDirection
	call DrawPlayer ; Draw the player in the updated position
	
	call MoveEnemy1
	; call MoveEnemy2
	; call MoveEnemy3
	; call MoveEnemy4
	@@cont:
	cmp [isExit], 1
	je @@exitGame
	jmp @@InputLoop
	@@exitGame:
	ret
endp
	

proc ChangeDirection
	PUSH_ALL
	mov ah, 1
	int 16h
	jz @@exit
	mov ah, 0
	int 16h
	
	cmp ah, 4bh ; Left Arrow
	je @@left
	cmp ah, 4dh ; Right Arrow
	je @@right
	cmp ah, 48h ; Up Arrow
	je @@up
	cmp ah, 50h ; Down Arrow
	je @@down
	cmp ah, 01 ; ESC
	je @@stop_game ; If pressed ESC - stop the game
	jmp @@exit
	
	@@left:
	cmp [playerDirection], 1 ; Prevent the player from reversing
	je @@exit
	mov [playerDirection], 3
	jmp @@exit
	
	@@right:
	cmp [playerDirection], 3 ; Prevent the player from reversing
	je @@exit
	mov [playerDirection], 1
	jmp @@exit
	
	@@up:
	cmp [playerDirection], 2 ; Prevent the player from reversing
	je @@exit
	mov [playerDirection], 0
	jmp @@exit
	
	@@down:
	cmp [playerDirection], 0 ; Prevent the player from reversing
	je @@exit
	mov [playerDirection], 2
	
	@@exit:
	POP_ALL
	ret
	
	@@stop_game:
	mov [isExit], 1
	jmp @@exit
endp

; ----------------------------------------------
; ---------------PLAYER FUNCTIONS---------------
; ----------------------------------------------

; Description: Function to calculate the offset of the player in the Grid
; INPUT: none
; OUTPUT: BX
proc PlayerCoordsToGrid
	push [playerX]
	push [playerY]
	call CoordinatesToGrid ; Calculates the current offset of the player on our grid
	pop bx
	ret
endp

proc UpdatePlayer
	PUSH_ALL
	call PlayerCoordsToGrid 
	mov al, 1
	not al
	and al, [bx] 
	mov [bx], al; Updates the player bit in the old byte to FALSE
	cmp [playerDirection], 0
	je @@up
	cmp [playerDirection], 1
	je @@right
	cmp [playerDirection], 2
	je @@down
	cmp [playerDirection], 3
	je @@left
	
	@@up:
	sub [playerY], PLAYER_SPEED
	jmp @@exit_func
	
	@@right:
	add [playerX], PLAYER_SPEED
	jmp @@exit_func
	
	@@down:
	add [playerY], PLAYER_SPEED
	jmp @@exit_func
	
	@@left:
	sub [playerX], PLAYER_SPEED
	jmp @@exit_func
	@@exit_func:
	call PlayerCoordsToGrid
	mov al, 1
	or al, [bx]
	mov [bx], al ; Updates the player bit in the new byte to TRUE
	POP_ALL
	ret
endp

; Draws black square using PutMatrixInScreen
proc ErasePlayer
	PUSH_ALL
	lea cx, [black_matrix]
	mov [matrix], cx
	push [playerX] ; Pass X for CoordinatesToVideo
	push [playerY] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp

; Draws the player using PutMatrixInScreen
proc DrawPlayer
	PUSH_ALL
	lea cx, [player_matrix]
	mov [matrix], cx
	push [playerX]
	push [playerY]
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp
	


; Delays the program for the passed amount of time
; Usage: Pass by value the amount of MILISECONDS
; IMPORTANT: Set up a "Timer" variable to count the iterations
proc LoopDelay
push bp 
mov bp , sp 
    push ax 
    push cx
    mov cx ,[bp+4]
@@Self1:
	  inc [timer]
      push cx
    mov cx,3000
@@Self2:
    loop @@Self2
    pop cx
    loop @@Self1
    pop cx
    pop ax 
    pop bp 
    ret 2
endp LoopDelay


; push the X
; push the Y
; result in stack
; Description: Function that calculates the needed Offset in video segment using the given coordinates
proc CoordinatesToGrid
	PUSH_ALL_BP
	mov bx, 200/PLAYER_LENGTH
	mov ax, [bp+4]
	xor dx, dx
	mul bx
	xor dx, dx
	add ax, [bp+6]
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp

proc PrintNum
    push bp
    mov bp, sp
    push ax ; | Prevent
    push bx ; | Ruining
    push cx ; | Outside
    push dx ; | Values
    
    xor dx, dx
    mov dx, [bp+4]
    mov ax, dx ; The number to print
    mov cx, 0
    @@myLoop:
    xor bh, bh
    xor dx, dx ; Reset DX because it affects the division result
    mov bl, 10 
    div bx ; al = ax / bl , ah = ax % bl
    push dx ; Save the most right digit
    inc cx ; Increase the counter of digits
    cmp ax, 0 ; Check if we have reached 0
    jne @@myLoop ; If we didn't - keep on going, if we did - Print
    
    @@Print:
    mov ah, 02h
    xor dx, dx 
    pop dx ; Get the last digit we pushed (most left digit)
    add dl, 30h ; Transform to ascii
    int 21h 
    loop @@Print
    
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp PrintNum

proc PrintNumWithColor
	PUSH_ALL_BP

    xor dx, dx
    mov dx, [bp+6]
    mov ax, dx ; The number to print
    mov cx, 0
    @@myLoop:
    xor bh, bh
    xor dx, dx ; Reset DX because it affects the division result
    mov bl, 10 
    div bx ; al = ax / bl , ah = ax % bl
    push dx ; Save the most right digit
    inc cx ; Increase the counter of digits
    cmp ax, 0 ; Check if we have reached 0
    jne @@myLoop ; If we didn't - keep on going, if we did - Print
    
    @@Print:
    mov bh, 0
	mov bl, [bp+4]
    xor ax, ax 
    pop ax ; Get the last digit we pushed (most left digit)
    add al, 30h ; Transform to ascii
	mov ah, 0eh
    int 10h 
    loop @@Print
    
	@@exit_func:
	POP_ALL_BP
	ret 4
endp

; Description: Prints to screen the number in HEXADECIMAL
; INPUT: number through stack

proc PrintHexaNumber
	PUSH_ALL_BP
	mov cx, [bp+4]
	cmp ch, 0
	jne @@ConvertToASCII
	shl cx, 8
	@@ConvertToASCII:
	mov dl, ch
	shr dl, 4
	add dl, 30h
	cmp dl, '9'
	jbe @@PrintDigit
	add dl, 7
	
	@@PrintDigit:
	mov ah, 0eh
	mov al, dl
	mov bl, 0fh
	int 10h
	shl cx, 4
	cmp cx, 0
	jne @@ConvertToASCII
	
	@@exit_func:
	POP_ALL_BP
	ret 2
endp

; Description: Prints to screen the number in HEXADECIMAL
; INPUT: number through stack, color through stack

proc PrintHexaNumberWithColor
	PUSH_ALL_BP
	mov di, 4
	mov cx, [bp+6]
	cmp ch, 0
	jne @@ConvertToASCII
	shl cx, 8
	mov di, 2
	@@ConvertToASCII:
	mov dl, ch
	shr dl, 4
	add dl, 30h
	cmp dl, '9'
	jbe @@PrintDigit
	add dl, 7
	
	@@PrintDigit:
	mov ah, 0eh
	mov al, dl
	mov bl, [bp+4]
	int 10h
	shl cx, 4
	dec di
	cmp di, 0
	jne @@ConvertToASCII
	
	
	
	@@exit_func:
	POP_ALL_BP
	ret 4
endp

; Description: moves cursor to given positiong
; INPUT: X coordinate through stack, Y coordinate through stack

proc MoveCursor
	PUSH_ALL_BP
	mov ah, 02h
	mov bh, 0
	mov dh, [bp+6]
	mov dl, [bp+4]
	int 10h
	POP_ALL_BP
	ret 4
endp


; ----------------------------------------------
; ---------------ENEMY FUNCTIONS----------------
; ----------------------------------------------

	
; Generates random spawn location and direction for the enemy
; Uses the RandomByCS function
; INPUT: offset of EnemyX, offset of EnemyY, offset of EnemyDirection
proc GenerateEnemyLocation
	PUSH_ALL_BP
	EnemyDirection equ [bp+4]
	EnemyY equ [bp+6]
	EnemyX equ [bp+8]
	
	; Generate X
	mov bl, 0
	mov bh, 320/PLAYER_LENGTH
	call RandomByCS
	xor ah, ah
	mov bx, EnemyX
	mov [bx], ax
	
	; Generate Y
	mov bl, 0
	mov bh, 200/PLAYER_LENGTH
	call RandomByCS
	xor ah, ah
	mov bx, EnemyY
	mov [bx], ax
	
	; Generate direction
	mov bl, 0
	mov bh, 3
	call RandomByCS
	xor ah, ah
	mov bx, EnemyDirection
	mov [bx], ax
	
	POP_ALL_BP
	ret 6
endp

; Description: Draws the given Enemy(since we have multiple) in the given coordinates 
; Input: Enemy X through stack, Enemy Y through stack
proc DrawEnemy
	PUSH_ALL_BP
	lea cx, [enemy_matrix]
	mov [matrix], cx
	push [bp+6]
	push [bp+4]
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL_BP
	ret 4
endp

; Description: Update the bit of the enemy in the grid, update enemy's location
; Input: offset Enemy X ,offset Enemy Y, Enemy Direction, Enemy bit value(16, 32, 64, 128)
proc UpdateEnemy
	PUSH_ALL_BP
	EnemyX equ [bp+10]
	EnemyY equ [bp+8]
	mov di, EnemyX
	mov si, EnemyY
	
	push [di]
	push [si]
	call CoordinatesToGrid ; Calculate the current/old Enemy offset on the Grid
	pop bx ; returns the offset to bx
	mov ax, [bp+4]
	not al
	and al, [bx]
	mov [bx], al ; Updates the enemy bit in the old byte to FALSE
	cmp [word bp+6], 0
	je @@up_right
	cmp [word bp+6], 1
	je @@down_right
	cmp [word bp+6], 2
	je @@down_left
	cmp [word bp+6], 3
	je @@up_left
	
	@@up_right:
	sub [word si], ENEMY_SPEED ; Move up
	add [word di], ENEMY_SPEED ; Move right
	jmp @@exit_func
	
	@@down_right:
	add [word di], ENEMY_SPEED ; Move right
	add [word si], ENEMY_SPEED ; Move down
	jmp @@exit_func
	
	@@down_left:
	add [word si], ENEMY_SPEED ; Move down
	sub [word di], ENEMY_SPEED ; Move left
	jmp @@exit_func
	
	@@up_left:
	sub [word di], ENEMY_SPEED ; Move left
	sub [word si], ENEMY_SPEED ; Move up
	jmp @@exit_func
	@@exit_func:
	push [di]
	push [si]
	call CoordinatesToGrid ; Calculate the current/old Enemy offset on the Grid
	pop bx ; returns the offset to bx
	mov ax, [bp+4]
	or al, [bx] 
	mov [bx], al ; Updates the enemy bit in the new byte to TRUE
	POP_ALL_BP
	ret 8
endp

proc UpdateEnemy1
	push bx
	push offset Enemy1X
	push offset Enemy1Y
	mov bl, [Enemy1Direction]
	xor bh, bh
	push bx
	push 16
	call UpdateEnemy
	pop bx
	ret
endp

; Draws black square using PutMatrixInScreen
proc EraseEnemy1
	PUSH_ALL
	lea cx, [black_matrix]
	mov [matrix], cx
	push [Enemy1X] ; Pass X for CoordinatesToVideo
	push [Enemy1Y] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp

proc UpdateEnemy2
	push bx
	push offset Enemy2X
	push offset Enemy2Y
	mov bl, [Enemy2Direction]
	xor bh, bh
	push bx
	push 32
	call UpdateEnemy
	pop bx
	ret
endp

; Draws black square using PutMatrixInScreen
proc EraseEnemy2
	PUSH_ALL
	lea cx, [black_matrix]
	mov [matrix], cx
	push [Enemy2X] ; Pass X for CoordinatesToVideo
	push [Enemy2Y] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp


proc UpdateEnemy3
	push bx
	push offset Enemy3X
	push offset Enemy3Y
	mov bl, [Enemy3Direction]
	xor bh, bh
	push bx
	push 64
	call UpdateEnemy
	pop bx
	ret
endp

; Draws black square using PutMatrixInScreen
proc EraseEnemy3
	PUSH_ALL
	lea cx, [black_matrix]
	mov [matrix], cx
	push [Enemy3X] ; Pass X for CoordinatesToVideo
	push [Enemy3Y] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp

proc UpdateEnemy4
	push bx
	push offset Enemy4X
	push offset Enemy4Y
	mov bl, [Enemy4Direction]
	xor bh, bh
	push bx
	push 128
	call UpdateEnemy
	pop bx
	ret
endp

; Draws black square using PutMatrixInScreen
proc EraseEnemy4
	PUSH_ALL
	lea cx, [black_matrix]
	mov [matrix], cx
	push [Enemy4X] ; Pass X for CoordinatesToVideo
	push [Enemy4Y] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL
	ret
endp

proc MoveEnemy1
	call EraseEnemy1
	call UpdateEnemy1
	push [Enemy1X]
	push [Enemy1Y]
	call DrawEnemy
	ret
endp

proc MoveEnemy2
	call EraseEnemy2
	call UpdateEnemy2
	push [Enemy2X]
	push [Enemy2Y]
	call DrawEnemy
	ret
endp

proc MoveEnemy3
	call EraseEnemy3
	call UpdateEnemy3
	push [Enemy3X]
	push [Enemy3Y]
	call DrawEnemy
	ret
endp

proc MoveEnemy4
	call EraseEnemy4
	call UpdateEnemy4
	push [Enemy4X]
	push [Enemy4Y]
	call DrawEnemy
	ret
endp

proc CreateEnemy1
	PUSH_ALL
	push offset Enemy1X
	push offset Enemy1Y
	push offset Enemy1Direction
	call GenerateEnemyLocation
	
	call UpdateEnemy1
	
	push [Enemy1X]
	push [Enemy1Y]
	call DrawEnemy
	POP_ALL
	ret
endp

proc CreateEnemy2
	PUSH_ALL
	push offset Enemy2X
	push offset Enemy2Y
	push offset Enemy2Direction
	call GenerateEnemyLocation
	
	push [Enemy2X]
	push [Enemy2Y]
	call DrawEnemy
	POP_ALL
	ret
endp

proc CreateEnemy3
	PUSH_ALL
	push offset Enemy3X
	push offset Enemy3Y
	push offset Enemy3Direction
	call GenerateEnemyLocation
	
	push [Enemy3X]
	push [Enemy3Y]
	call DrawEnemy
	POP_ALL
	ret
endp

proc CreateEnemy4
	PUSH_ALL
	push offset Enemy4X
	push offset Enemy4Y
	push offset Enemy4Direction
	call GenerateEnemyLocation
	
	push [Enemy4X]
	push [Enemy4Y]
	call DrawEnemy
	POP_ALL
	ret
endp


; ----------------------------------------------
; --------------------RANDOM--------------------
; ----------------------------------------------


; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the nask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs


; make mask acording to bh size 
; output Si = mask put 1 in all bh range
; example  if bh 4 or 5 or 6 or 7 si will be 7
; 		   if Bh 64 till 127 si will be 127
Proc MakeMask    
    push bx

	mov si,1
    
@@again:
	shr bh,1
	cmp bh,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop bx
	ret
endp  MakeMask


EndOfCsLbl:
END start
