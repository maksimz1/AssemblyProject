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
MOVE_DELAY = 150
MIN_X = 0
MIN_Y = 0
MAX_X = 63
MAX_Y = 39
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

; grid db (320/PLAYER_LENGTH)*(200/PLAYER_LENGTH) dup(0) ; 64 * 40
grid db (320/PLAYER_LENGTH)*(200/PLAYER_LENGTH/2-1) dup(0)
	 db 2 dup((320/PLAYER_LENGTH/8) dup(0), (320/PLAYER_LENGTH/8) dup(00000100b),(320/PLAYER_LENGTH/8)*6 dup(0))
	 db (320/PLAYER_LENGTH)*(200/PLAYER_LENGTH/2-1) dup(0)
	 
player_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Ah)
trail_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Bh)
enemy_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Ch)
wall_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Dh)

black_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup (0)
matrix dw ?

playerX dw 5 ; Player's Location relative to GRID
playerY dw 5
playerDirection db 0 ; Directions of movement: 0 - up, 1 - right, 2 - down, 3 - left

Enemy1X dw 20
Enemy1Y dw 2
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

; in dx how many cols (horizontal)
; in cx how many rows (vertical)
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
	mov bx, 64*PLAYER_LENGTH*PLAYER_LENGTH
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
	call CreateEnemy2
	call CreateEnemy3
	call DrawWalls

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
	

	call ErasePlayer ; Erase the outdated position of the Player
	call UpdatePlayer ; Updates Player's position according to playerDirection
	call DrawPlayer ; Draw the player in the updated position
	
	call MoveEnemy1
	call MoveEnemy2
	call MoveEnemy3

	@@cont:
	cmp [isExit], 1
	je @@exitGame
	jmp @@InputLoop
	@@exitGame:
	ret
endp
	
proc DrawWall
	PUSH_ALL_BP
	lea cx, [wall_matrix]
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

proc DrawWalls
	PUSH_ALL
	mov cx,320/PLAYER_LENGTH-1 ; Holds the X
	@@x_axis:
	push cx 
	mov di, cx
	mov cx, 200/PLAYER_LENGTH-1 ; Holds the Y
	@@y_axis:
	push di ; Pass the X to the function
	push cx ; Pass the Y to the function
	call CheckWallBit
	pop bx ; Get Function Output
	cmp bx, 1
	jne @@cont
	@@draw_wall:
	push di
	push cx
	call DrawWall
	@@cont:
	loop @@y_axis
	pop cx 
	loop @@x_axis
	POP_ALL
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
	jmp @@exit
	
	@@stop_game:
	mov [isExit], 1
	jmp @@exit
	
	@@exit:
	POP_ALL
	ret
endp

; ----------------------------------------------
; ---------------PLAYER FUNCTIONS---------------
; ----------------------------------------------



; OUTPUT: AX - Boolean value(1 - Legal, 0 - Illegal)
proc PlayerCheckLegalMove
	cmp [playerDirection], 0
	je @@up
	cmp [playerDirection], 1
	je @@right
	cmp [playerDirection], 2
	je @@down
	cmp [playerDirection], 3
	je @@left
	
	@@up:
	mov ax, [playerY]
	sub ax, PLAYER_SPEED
	cmp ax, MIN_Y
	jge @@Legal
	jmp @@Illegal
	
	@@right:
	mov ax, [playerX]
	add ax, PLAYER_SPEED
	cmp ax, MAX_X
	jbe @@Legal
	jmp @@Illegal

	@@down:
	mov ax, [playerY]
	add ax, PLAYER_SPEED
	cmp ax, MAX_Y
	jle @@Legal
	jmp @@Illegal

	@@left:
	mov ax, [playerX]
	sub ax, PLAYER_SPEED
	cmp ax, MIN_X
	jge @@Legal
	jmp @@Illegal
	
	@@Legal:
	mov ax, 1
	jmp @@exit_func
	@@Illegal:
	mov ax, 0
	
	@@exit_func:
	ret
endp
	
; Description: Disable a given bit in a given byte using NOT and AND logical operations
; INPUT: BX - Offset of byte on grid, AL - Bit to disable
proc DisableBit
	not al
	and al, [bx] 
	mov [bx], al
	ret
endp 
; Description: Enable a given bit in a given byte using OR logical operation
; INPUT: BX - Offset of byte to, AL - Bit to Enable
proc EnableBit
	or al, [bx]
	mov [bx], al
	ret
endp 

; Description: Checks if a given bit in a given byte is enabled
;			   Using AND operation between the two values and if
;			   the operation result differs from 0, it is enabled
; INPUT: BX - Offset of byte on grid, AL - Bit to Check
; OUTPUT: AL - boolean(1 - Enabled, 0 - Disabled)
proc CheckBit
	and al, [bx]
	cmp al, 0
	jne @@Enabled
	jmp @@exit_func
	
	@@Enabled:
	mov al, 1
	
	@@exit_func:
	ret
endp

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
	call PlayerCheckLegalMove ; Checks if it is legal to move the player, boolean result in AX
	cmp ax, 1
	jne @@exit_func
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
	jmp @@UpdateGrid
	
	@@right:
	add [playerX], PLAYER_SPEED
	jmp @@UpdateGrid
	
	@@down:
	add [playerY], PLAYER_SPEED
	jmp @@UpdateGrid
	
	@@left:
	sub [playerX], PLAYER_SPEED
	
	@@UpdateGrid:
	call PlayerCoordsToGrid 
	mov al, 00000001b 
	call EnableBit
	
	@@exit_func:
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
	mov bx, 320/PLAYER_LENGTH
	mov ax, [bp+4]
	xor dx, dx
	mul bx ; 64 * Y
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

; DESCRIPTION: Checks if the wall bit is enabled in 
; 			   given grid cell, returns a boolean through AL
; INPUT: (X, Y) -> Through stack
; OUTPUT: boolean (1 - Enabled, 0 - Disabled) -> Through stack
proc CheckWallBit
	PUSH_ALL_BP
	push [bp+6]
	push [bp+4]
	call CoordinatesToGrid
	pop bx
	mov al, 00000100b
	call CheckBit
	xor ah, ah
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp
	

; ----------------------------------------------
; ---------------ENEMY FUNCTIONS----------------
; ----------------------------------------------

; INPUT: (Enemy X, Enemy Y, offset of EnemyDirection) -> Through Stack
; OUTPUT: Change of value in EnemyDirection(if needed)
proc EnemyChangeDirection
	PUSH_ALL_BP
	mov bx, [bp+4]
	cmp [byte bx], 0
	je @@jmp_up_right
	cmp [byte bx], 1
	je @@jmp_down_right
	cmp [byte bx], 2
	je @@jmp_down_left
	cmp [byte bx], 3
	je @@jmp_up_left
	
	@@jmp_up_right:
	jmp @@up_right
	@@jmp_down_right:
	jmp @@down_right
	@@jmp_down_left:
	jmp @@down_left
	@@jmp_up_left:
	jmp @@up_left
	@@jmp_exit_func:
	jmp @@exit_func
	
	@@up_right:
	push [bp+8]
	push [bp+6]
	call CheckUpRight
	pop ax
	cmp ax, 1
	je @@jmp_exit_func
	push [bp+8]
	push [bp+6]
	call CheckDownRight
	pop ax
	cmp ax, 1
	je @@jmp_set_down_right
	push [bp+8]
	push [bp+6]
	call CheckUpLeft
	pop ax
	cmp ax, 1
	je @@jmp_set_up_left
	jmp @@set_down_left
	
	@@down_right:
	push [bp+8]
	push [bp+6]
	call CheckDownRight
	pop ax
	cmp ax, 1
	je @@jmp_exit_func
	push [bp+8]
	push [bp+6]
	call CheckUpRight
	pop ax
	cmp ax, 1
	je @@set_up_right
	push [bp+8]
	push [bp+6]
	call CheckDownLeft
	pop ax
	cmp ax, 1
	je @@set_down_left
	jmp @@set_up_left
	
	@@jmp_set_down_right:
	jmp @@set_down_right
	@@jmp_set_up_left:
	jmp @@set_up_left
	
	
	@@up_left:
	push [bp+8]
	push [bp+6]
	call CheckUpLeft
	pop ax
	cmp ax, 1
	je @@exit_func
	push [bp+8]
	push [bp+6]
	call CheckDownLeft
	pop ax
	cmp ax, 1
	je @@set_down_left
	push [bp+8]
	push [bp+6]
	call CheckUpRight
	pop ax
	cmp ax, 1
	je @@set_up_right
	jmp @@set_down_right
	
	@@down_left:
	push [bp+8]
	push [bp+6]
	call CheckDownLeft
	pop ax
	cmp ax, 1
	je @@exit_func
	push [bp+8]
	push [bp+6]
	call CheckUpLeft
	pop ax
	cmp ax, 1
	je @@set_up_left
	push [bp+8]
	push [bp+6]
	call CheckDownRight
	pop ax
	cmp ax, 1
	je @@set_down_right
	jmp @@set_up_right
	
	@@set_up_right:
	mov bx, [bp+4]
	mov [byte bx], 0
	jmp @@exit_func
	@@set_down_right:
	mov bx, [bp+4]
	mov [byte bx], 1
	jmp @@exit_func
	@@set_down_left:
	mov bx, [bp+4]
	mov [byte bx], 2
	jmp @@exit_func
	@@set_up_left:
	mov bx, [bp+4]
	mov [byte bx], 3
	jmp @@exit_func
	
	@@exit_func:
	POP_ALL_BP
	ret 6
endp

; DESCRIPTION: SubFunction that checks if it is legal for the
; 			   Enemy to move Up Right, result through stack
; INPUT: (Enemy X, Enemy Y) -> Through stack
; OUTPUT: boolean (1 - Legal, 0 - Illegal) -> Through Stack
proc CheckUpRight
	PUSH_ALL_BP
	mov ax, [word bp+6]
	mov bx, [word bp+4]
	add ax, ENEMY_SPEED
	sub bx, ENEMY_SPEED
	
	cmp bx, MIN_Y
	jl @@Illegal ; If passing upper boundary, change direction
	cmp ax, MAX_X
	jg @@Illegal ; If passing right boundary, change direction
	push ax
	push bx
	call CheckWallBit
	pop ax
	cmp ax, 0
	je @@Legal
	
	@@Illegal:
	mov [word bp+6], 0
	jmp @@exit_func
	@@Legal:
	mov [word bp+6], 1
	
	@@exit_func:
	POP_ALL_BP
	ret 2
endp

; DESCRIPTION: SubFunction that checks if it is legal for the
; 			   Enemy to move Down Right, result through stack
; INPUT: (Enemy X, Enemy Y) -> Through stack
; OUTPUT: boolean (1 - Legal, 0 - Illegal) -> Through Stack
proc CheckDownRight
	PUSH_ALL_BP
	mov ax, [word bp+6]
	mov bx, [word bp+4]
	add ax, ENEMY_SPEED
	add bx, ENEMY_SPEED
	
	cmp bx, MAX_Y
	jg @@Illegal ; If passing bottom boundary, change direction
	cmp ax, MAX_X
	jg @@Illegal ; If passing right boundary, change direction
	push ax
	push bx
	call CheckWallBit
	pop ax
	cmp ax, 0
	je @@Legal
	
	@@Illegal:
	mov [word bp+6], 0
	jmp @@exit_func
	@@Legal:
	mov [word bp+6], 1
	
	@@exit_func:
	POP_ALL_BP
	ret 2
endp

; DESCRIPTION: SubFunction that checks if it is legal for the
; 			   Enemy to move Up Left, result through stack
; INPUT: (Enemy X, Enemy Y) -> Through stack
; OUTPUT: boolean (1 - Legal, 0 - Illegal) -> Through Stack
proc CheckUpLeft
	PUSH_ALL_BP
	mov ax, [word bp+6]
	mov bx, [word bp+4]
	sub ax, ENEMY_SPEED
	sub bx, ENEMY_SPEED
	
	cmp bx, MIN_Y
	jl @@Illegal ; If passing upper boundary, change direction
	cmp ax, MIN_X
	jl @@Illegal ; If passing left boundary, change direction
	push ax
	push bx
	call CheckWallBit
	pop ax
	cmp ax, 0
	je @@Legal
	
	@@Illegal:
	mov [word bp+6], 0
	jmp @@exit_func
	@@Legal:
	mov [word bp+6], 1
	
	@@exit_func:
	POP_ALL_BP
	ret 2
endp
; DESCRIPTION: SubFunction that checks if it is legal for the
; 			   Enemy to move Down Left, result through stack
; INPUT: (Enemy X, Enemy Y) -> Through stack
; OUTPUT: boolean (1 - Legal, 0 - Illegal) -> Through Stack
proc CheckDownLeft
	PUSH_ALL_BP
	mov ax, [word bp+6]
	mov bx, [word bp+4]
	sub ax, ENEMY_SPEED
	add bx, ENEMY_SPEED
	
	cmp bx, MAX_Y 
	jg @@Illegal ; If passing bottom boundary, change direction
	cmp ax, MIN_X
	jl @@Illegal ; If passing left boundary, change direction
	push ax
	push bx
	call CheckWallBit
	pop ax
	cmp ax, 0
	je @@Legal
	
	@@Illegal:
	mov [word bp+6], 0
	jmp @@exit_func
	@@Legal:
	mov [word bp+6], 1
	
	@@exit_func:
	POP_ALL_BP
	ret 2
endp

	
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
	mov bh, 320/PLAYER_LENGTH-1
	call RandomByCS
	xor ah, ah
	mov bx, EnemyX
	mov [bx], ax
	
	; Generate Y
	mov bl, 0
	mov bh, 200/PLAYER_LENGTH-1
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

; Description: Draws an Enemy in the given coordinates 
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
; Input: offset Enemy X ,offset Enemy Y, offset Enemy Direction, Enemy bit value(16, 32, 64, 128)
proc UpdateEnemy
	PUSH_ALL_BP
	EnemyX equ [bp+10]
	EnemyY equ [bp+8]
	EnemyDirection equ [bp+6]
	
	mov di, EnemyX
	mov si, EnemyY
	mov bx, EnemyDirection
	
	push [di]
	push [si]
	push bx
	call EnemyChangeDirection
	
	push [di]
	push [si]
	call CoordinatesToGrid ; Calculate the current/old Enemy offset on the Grid
	pop bx ; returns the offset to bx
	mov ax, [bp+4]
	call DisableBit
	
	mov bx, EnemyDirection
	cmp [byte bx], 0
	je @@up_right
	cmp [byte bx], 1
	je @@down_right
	cmp [byte bx], 2
	je @@down_left
	cmp [byte bx], 3
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
	call EnableBit
	POP_ALL_BP
	ret 8
endp

proc UpdateEnemy1
	push offset Enemy1X
	push offset Enemy1Y
	push offset Enemy1Direction
	push 16
	call UpdateEnemy
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
	push offset Enemy2Direction
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
	push offset Enemy3Direction
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
	push offset Enemy4Direction
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
