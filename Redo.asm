IDEAL
MODEL small
STACK 4000h

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
; --------- 
PLAYER_BIT = 00000001b
TRAIL_BIT = 00000010b
WALL_BIT = 00000100b
OUTER_BIT = 00001000b
ENEMY1_BIT = 00010000b
ENEMY2_BIT = 00100000b
ENEMY3_BIT = 01000000b
ENEMY4_BIT = 10000000b
PLAYER_LENGTH = 5
PLAYER_SPEED = 1
ENEMY_SPEED = 1
MOVE_DELAY = 100
ENEMY_MOVE_DELAY = 100	
MIN_X = 0
MIN_Y = 0
MAX_X = 320/PLAYER_LENGTH-1
MAX_Y = 200/PLAYER_LENGTH-1
MENU_BUTTON_X=137

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

player_matrix db 000h,0E0h,0E0h,0E0h,000h,0E0h,0E0h,0E0h,0E0h,0E0h
			db 0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h
			db 000h,0E0h,0E0h,0E0h,000h
player_wall_matrix db 039h,0E0h,0E0h,0E0h,039h,0E0h,0E0h,0E0h,0E0h,0E0h
			db 0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h,0E0h
			db 039h,0E0h,0E0h,0E0h,039h
trail_matrix db 0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh
			db 0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh
			db 0FEh,0FEh,0FEh,0FEh,0FEh
trail_vert_matrix db 000h,000h,000h,000h,000h,0FEh,0FEh,0FEh,0FEh,0FEh
		db 0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh,0FEh
		db 000h,000h,000h,000h,000h
trail_hor_matrix db 000h,0FEh,0FEh,0FEh,000h,000h,0FEh,0FEh,0FEh,000h
		db 000h,0FEh,0FEh,0FEh,000h,000h,0FEh,0FEh,0FEh,000h
		db 000h,0FEh,0FEh,0FEh,000h
trail1_matrix db 000h,000h,000h,000h,000h,0FEh,0FEh,0FEh,0FEh,000h
		db 0FEh,0FEh,0FEh,0FEh,000h,0FEh,0FEh,0FEh,0FEh,000h
		db 000h,0FEh,0FEh,0FEh,000h
trail2_matrix db 000h,000h,000h,000h,000h,000h,0FEh,0FEh,0FEh,0FEh
		db 000h,0FEh,0FEh,0FEh,0FEh,000h,0FEh,0FEh,0FEh,0FEh
		db 000h,0FEh,0FEh,0FEh,000h
trail3_matrix db 000h,0FEh,0FEh,0FEh,000h,0FEh,0FEh,0FEh,0FEh,000h
		db 0FEh,0FEh,0FEh,0FEh,000h,0FEh,0FEh,0FEh,0FEh,000h
		db 000h,000h,000h,000h,000h
trail4_matrix db 000h,0FEh,0FEh,0FEh,000h,000h,0FEh,0FEh,0FEh,0FEh
		db 000h,0FEh,0FEh,0FEh,0FEh,000h,0FEh,0FEh,0FEh,0FEh
		db 000h,000h,000h,000h,000h

enemy_matrix db 000h,0F9h,0F9h,0F9h,000h,0F9h,0F9h,0F9h,0F9h,0F9h
		db 0F9h,0F9h,0F9h,0F9h,0F9h,0F9h,0F9h,0F9h,0F9h,0F9h
		db 000h,0F9h,0F9h,0F9h,000h
wall_matrix db 039h,039h,039h,039h,039h,039h,039h,039h,039h,039h
			db 039h,039h,039h,039h,039h,039h,039h,039h,039h,039h
			db 039h,039h,039h,039h,039h
outer_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup(0Dh)
black_matrix db PLAYER_LENGTH*PLAYER_LENGTH dup (0)
matrix dw ?


playerX dw 5 ; Player's Location relative to GRID
playerY dw 5
playerDirection db 0 ; Directions of movement: 0 - up, 1 - right, 2 - down, 3 - left
oldPlayerDirection db 0 ; previous Direction of movement

Level db 3

IsDrawingTrail db 0

Enemy1X dw 20
Enemy1Y dw 2
Enemy1Direction db 0

Enemy2X dw MAX_X+1
Enemy2Y dw MAX_Y
Enemy2Direction db 0

Enemy3X dw MAX_X+1
Enemy3Y dw MAX_Y
Enemy3Direction db 0

Enemy4X dw MAX_X+1
Enemy4Y dw MAX_Y
Enemy4Direction db 0

gameStatus db 0 ; 0 - Still playing, 1 - Lost, 2 - Won
isExit db 0
RndCurrentPos dw 0
timer dw 0
second_counter dw 0

menuSelection db 0

;BMP File data
OneBmpLine 	db 200 dup (0)  ; One Color line read buffer
   
ScrLine 	db 320 dup (0)  ; One Color line read buffer

MenuFileName db "menu0.bmp",0
FileHandle	dw ?
Header 	    db 54 dup(0)
BmpFileErrorMsg    	db 'Error At Opening Bmp File ','start.bmp', 0dh, 0ah,'$'
ErrorFile           db 0
Palette 	db 400h dup (0)

BmpLeft dw ?
BmpTop dw ?
BmpColSize dw ?
BmpRowSize dw ?

CODESEG
start:
	mov ax, @data
	mov ds, ax
	mov ax, 13h
	int 10h	
	
	call StartMenu	
	
exit:
	mov ax, 4c00h
	int 21h

; DESCRIPTION: Draws all the Start Menu buttons
proc StartMenu
	
	PUSH_ALL
	; SETUP FOR DRAWING MENU
	mov [BmpLeft], 0
	mov [BmpTop], 0
	mov [BmpColSize], 320
	mov [BmpRowSize], 200
	mov dx, offset MenuFileName
	call OpenShowBmp
	; LOOP FOR TAKING INPUT
	@@InputLoop:
	
	mov ah, 1
	int 16h
	jz @@InputLoop
	mov ah, 0
	int 16h
	
	
	cmp ah, 48h ; Up Arrow
	je @@SelectionUp
	cmp ah, 50h ; Down Arrow
	je @@SelectionDown
	
	
	cmp ah, 01ch
	je @@EnterSelection
	
	cmp ah, 39h
	je @@EnterSelection
	
	jmp @@InputLoop
	
	@@EnterSelection:
	cmp [menuSelection], 0
	je @@StartGame
	
	cmp [menuSelection], 2
	je @@exit
	
	@@StartGame:
	call PlayGame
	jmp @@exit
	
	@@SelectionUp:
		cmp [menuSelection], 0
		je @@InputLoop
		dec [menuSelection]
		mov al, [menuSelection]
		add al, 30h
		mov bx, offset MenuFileName
		add bx, 4
		mov [bx], al
		mov dx, offset MenuFileName
		call OpenShowBmp
		jmp @@InputLoop
	
	@@SelectionDown:
		cmp [menuSelection], 2
		je @@InputLoop
		inc [menuSelection]
		mov al, [menuSelection]
		add al, 30h
		mov bx, offset MenuFileName
		add bx, 4
		mov [bx], al
		mov dx, offset MenuFileName
		call OpenShowBmp
		jmp @@InputLoop
		
		
	
	@@exit:
	POP_ALL
	ret
endp

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

proc PlayLevel1
	call CreateEnemy1
	mov [Level], 1
	call PlayGame
	ret
endp

proc PlayLevel2
	call CreateEnemy2
	mov [Level], 2
	call PlayGame
	ret
endp

proc PlayLevel3
	call CreateEnemy3
	mov [Level], 3
	call PlayGame
	ret
endp

proc PlayLevel4
	call CreateEnemy4
	mov [Level], 4
	call PlayGame
	ret
endp

; Main Function and Loop of Game	
proc PlayGame
	; ------------ Initialization
	call EraseScreen
	push [playerX]
	push [playerY]
	call CreateWalls
	call DrawWalls
	call CreateEnemies
	; ------------- End of Initialization
	
	@@InputLoop:
	push 1
	call TimeBasedDelay ; Makes a delay for one milisecond
	call ChangeDirection ; Takes input and changes direction accordingly
	
	; Makes the delay for updating and drawing the enemy
	@@enemyDelayCheck:
	mov ax, [timer] 
	mov bx, ENEMY_MOVE_DELAY
	xor dx, dx
	div bx
	cmp dx, 0
	je @@enemyMove
	jmp @@playerDelayCheck
	
	@@enemyMove:
	; Moves and redraws the enemies (according to the level)
	call MoveEnemies
	@@cont3:
	call CheckLose ; After moving the enemies, Checks if they hit the player ot the trail. AL holds the result
	cmp al, 1
	je @@Mid_LoseGame
	
	; Makes the delay for updating and drawing the player
	@@playerDelayCheck:
	mov ax, [timer] 
	mov bx, MOVE_DELAY
	xor dx, dx
	div bx
	cmp dx, 0
	je @@playerMove ; If 100 Miliseconds haven't passed, keep counting
	jmp @@cont2
	@@Mid_LoseGame:
	jmp @@LoseGame ; did this to avoid 'Relative Jump Out of Range'
	@@playerMove:
	call AddTrail
	cmp ax, 1 ; Checks if we captured new area
	jne @@cont1
	call AddArea
	@@cont1:
	push [playerX]
	push [playerY]
	call RefreshCell
	call UpdatePlayer ; Updates Player's position according to playerDirection
	
	; Draws the player with the correct background by checking if there is a wall in the background
	push [playerX]
	push [playerY]
	call CheckWallBit
	pop ax
	cmp ax, 0
	je @@RegularBackground
	push [playerX]
	push [playerY]
	push offset player_wall_matrix
	call DrawMatrix
	jmp @@cont2
	@@RegularBackground:
	push [playerX]
	push [playerY]
	push offset player_matrix
	call DrawMatrix ; Draw the player in the updated position
	
	; Saves the current direction for next iteration
	mov bl, [playerDirection] 
	mov [oldPlayerDirection], bl
	
	@@cont2:
	cmp [isExit], 1
	je @@exitGame
	jmp @@InputLoop
	
	@@LoseGame:
	mov [gameStatus], 1
	mov [isExit], 1
	@@exitGame:
	ret
endp


proc DrawWalls
	PUSH_ALL
	mov cx, MAX_X
	mov di, 0
	@@outer_loop:
	push cx
	mov cx, MAX_Y
	mov si, 0
	@@inner_loop:
	push di
	push si
	call CheckWallBit
	pop ax
	cmp ax, 1
	jne @@draw_black

	@@draw_wall:
	push di
	push si
	push offset wall_matrix
	call DrawMatrix
	jmp @@cont
	
	@@draw_black:
	push [playerX]
	push [playerY]
	push offset black_matrix
	call DrawMatrix
	@@cont:
	inc si
	loop @@inner_loop
	pop cx
	inc di
	loop @@outer_loop
	POP_ALL
	ret
endp

; DESCRIPTION: Redraws all of the elements on the screen 
; We will use this command after capturing new area to update the captured area on the screen
proc EraseScreen
	PUSH_ALL
	mov cx, MAX_X
	mov di, 0
	@@outer_loop:
	push cx
	mov cx, MAX_Y
	mov si, 0
	@@inner_loop:
	push di
	push si
	push offset black_matrix
	call DrawMatrix	
	@@cont:
	inc si
	loop @@inner_loop
	pop cx
	inc di
	loop @@outer_loop
	POP_ALL
	ret 
endp

proc CreateWalls
	PUSH_ALL_BP
	mov cx, 3
	mov di, 0
	@@outer_loop:
	push cx
	mov si,0 
	mov cx, 3
	@@inner_loop:
	mov ax, [bp+6]
	add ax, si
	mov bx, [bp+4]
	add bx, di
	push ax
	push bx
	call CoordinatesToGrid
	pop bx
	mov al, WALL_BIT
	call EnableBit
	inc si
	loop @@inner_loop
	pop cx
	inc di
	loop @@outer_loop
	POP_ALL_BP
	ret 4
endp

; DESCRIPTION: Function To Add Trail piece to the current cell
;			   and returns a value to tell us if we should add New Area
; INPUT: None
; OUTPUT: Boolean(1-Add New Area, 0-Dont Add) -> AX
proc AddTrail
	push [playerX]
	push [playerY]
	call CheckWallBit
	pop ax
	cmp ax, 1
	je @@NoAddTrail
	jmp @@YesAddTrail
	
	@@NoAddTrail:
	cmp [IsDrawingTrail], 1
	jne @@NoAddTrail2
	
	@@NoAddTrail1:
	mov [IsDrawingTrail], 0
	mov ax, 1
	jmp @@exit_func
	
	@@NoAddTrail2:
	mov ax, 0
	jmp @@exit_func
	
	@@YesAddTrail:
	call PlayerCoordsToGrid 
	mov al, TRAIL_BIT 
	call EnableBit
	mov [IsDrawingTrail], 1
	mov ax, 0
	
	@@exit_func:
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

; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette		near					
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette
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

; ----------------------------------------------
; ---------------PLAYER FUNCTIONS---------------
; ----------------------------------------------

; DESCRIPTION: Checks for player loss
; How it works: FOR EACH ENEMY SEPERATELY
; 1) Gets the Enemy's current cell on the grid
; 2) Checks the cell for the Player bit and the trail bit
; If any of them is enabled - The player has lost
; OUTPUT: AL => Is the player lost (1-Lost, 0-Didnt lose)
proc CheckLose
	push [Enemy1X]
	push [Enemy1Y]
	call CoordinatesToGrid 
	pop bx ; BX => Cell of Enemy1 on the grid
	mov al, PLAYER_BIT + TRAIL_BIT 
	call CheckBit ; Checks if either the player or the trail is on the enemy cell
	; AL => Is any of the bits enabled(1-Enabled,0-Disabled)
	cmp al, 1
	je @@exit_func
	
	push [Enemy2X]
	push [Enemy2Y]
	call CoordinatesToGrid 
	pop bx ; BX => Cell of Enemy2 on the grid
	mov al, PLAYER_BIT + TRAIL_BIT 
	call CheckBit ; Checks if either the player or the trail is on the enemy cell
	; AL => Is any of the bits enabled(1-Enabled,0-Disabled)
	cmp al, 1
	je @@exit_func
	
	push [Enemy3X]
	push [Enemy3Y]
	call CoordinatesToGrid 
	pop bx ; BX => Cell of Enemy3 on the grid
	mov al, PLAYER_BIT + TRAIL_BIT 
	call CheckBit ; Checks if either the player or the trail is on the enemy cell
	; AL => Is any of the bits enabled(1-Enabled,0-Disabled)
	
	@@exit_func:
	ret
endp CheckLose

	

; DESCRIPTION: Add new "Walls"/Area to the part surrounded by the trail
; How it works:
; 1) Replaces all trail pieces with wall pieces(disables trail bit, enables wall bit)
; 2) Calls MarkOuterBit function from all 4 corners of the screen (Allowing us to mark all of the outside/unwanted area)
; 3) Iterates through all the cells on the grid and for each cell that's not marked as an outside/unwanted cell, Add a wall piece
; 4) Disables the outside bit for all the cells on the grid(allowing us to use the same method the next time we capture an area)
proc AddArea
	PUSH_ALL
	mov cx, (320/PLAYER_LENGTH)*(200/PLAYER_LENGTH)
	mov di, 0
	@@FindTrails:
	mov bx, di
	mov al, TRAIL_BIT
	call CheckBit
	cmp al, 1
	jne @@cont1
	mov al, TRAIL_BIT
	call DisableBit
	mov al, WALL_BIT
	call EnableBit
	@@cont1:
	inc di
	loop @@FindTrails
	
	push MIN_X
	push MIN_Y
	call MarkOuterBit
	push MAX_X
	push MIN_Y
	call MarkOuterBit
	push MAX_X
	push MAX_Y
	call MarkOuterBit
	push MIN_X
	push MAX_Y
	call MarkOuterBit
	
	mov cx, MAX_X+1
	mov di, 0
	@@outer_loop:
	push cx
	mov cx, MAX_Y+1
	mov si, 0
	@@inner_loop:
	push di
	push si
	call CoordinatesToGrid
	pop bx
	mov al, OUTER_BIT
	call CheckBit
	cmp al, 1
	je @@ClearMark
	
	@@SetWall:
	mov al, WALL_BIT
	call EnableBit
	push di
	push si
	push offset wall_matrix
	call DrawMatrix
	jmp @@cont
	
	@@ClearMark:
	mov al, OUTER_BIT
	call DisableBit
	
	@@cont:
	inc si
	loop @@inner_loop
	pop cx
	inc di
	loop @@outer_loop
	POP_ALL
	ret
endp
	
; DESCRIPTION: Marks All the outer area(The area we don't want to capture) using recursion
; INPUT: (X,Y)-> Through Stack
proc MarkOuterBit
	push bp
	mov bp, sp
	
	push [bp+6]
	push [bp+4]
	call CheckWallBit
	pop ax
	cmp ax, 1
	je @@mid_exit_func
	push [bp+6]
	push [bp+4]
	call CheckOuterBit
	pop ax
	cmp ax, 1
	je @@mid_exit_func
	
	push [bp+6]
	push [bp+4]
	call CoordinatesToGrid
	pop bx
	mov al, OUTER_BIT
	call EnableBit
	jmp @@xPlus1
	
	@@mid_exit_func:
	jmp @@exit_func
	
	@@xPlus1:
	mov ax, [bp+6]
	inc ax
	cmp ax, MAX_X
	jg @@yPlus1
	push ax
	push [bp+4]
	call  MarkOuterBit
	
	@@yPlus1:
	mov ax, [bp+4]
	inc ax
	cmp ax, MAX_Y
	jg @@xMinus1
	push [bp+6]
	push ax
	call  MarkOuterBit
	
	@@xMinus1:
	mov ax, [bp+6]
	dec ax
	cmp ax, MIN_X
	jl @@yMinus1
	push ax
	push [bp+4]
	call  MarkOuterBit
	
	@@yMinus1:
	mov ax, [bp+4]
	dec ax
	cmp ax, MIN_Y
	jl @@exit_func
	push [bp+6]
	push ax
	call  MarkOuterBit
	
	@@exit_func:
	pop bp
	ret 4 
endp	

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
	
	call PlayerCoordsToGrid 
	mov al, PLAYER_BIT 
	call DisableBit
	
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
	jmp @@AddPlayerToGrid
	
	@@right:
	add [playerX], PLAYER_SPEED
	jmp @@AddPlayerToGrid
	
	@@down:
	add [playerY], PLAYER_SPEED
	jmp @@AddPlayerToGrid
	
	@@left:
	sub [playerX], PLAYER_SPEED
	
	@@AddPlayerToGrid:
	call PlayerCoordsToGrid 
	mov al, PLAYER_BIT 
	call EnableBit
	
	@@exit_func:
	POP_ALL
	ret
endp

; Draws the a 5x5 matrix using PutMatrixInScreen
proc DrawMatrix
	PUSH_ALL_BP
	mov cx, [bp+4]
	mov [matrix], cx
	push [bp+8] ; Pass X for CoordinatesToVideo
	push [bp+6] ; Pass Y for CoordinatesToVideo
	call CoordinatesToVideo
	pop di
	mov cx, PLAYER_LENGTH
	mov dx, PLAYER_LENGTH
	call PutMatrixInScreen
	POP_ALL_BP
	ret 6
endp

	
; DESCRIPTION: Draws the trail matrix using PutMatrixInScreen
; 			   Draws the correct trail by checking current and old direction
; INPUT: (X, Y) -> Through Stack
; OUTPUT: Draws trail to screen at given coordinates 
proc DrawTrail
	PUSH_ALL_BP
	mov al, [playerDirection]
	mov ah, [oldPlayerDirection]
	cmp al, 0 ; current direction UP
	je @@CheckUp
	cmp al, 1 ; current direction RIGHT
	je @@CheckRight
	cmp al, 2 ; current direction DOWN
	je @@CheckDown
	cmp al, 3 ; current direction LEFT
	je @@CheckLeft
	
	@@CheckUp:
		cmp ah, 1
		je @@Case3
		cmp ah, 3
		je @@Case4
		jmp @@Horizontal
		
	@@CheckDown:
		cmp ah, 1
		je @@Case1
		cmp ah, 3
		je @@Case2
		jmp @@Horizontal
		
	@@CheckLeft:
		cmp ah, 0
		je @@Case1
		cmp ah, 2
		je @@Case3
		jmp @@Vertical
	
	@@CheckRight:
		cmp ah, 0
		je @@Case2
		cmp ah, 2
		je @@Case4
		jmp @@Vertical
		
	@@Case1:
	push [bp+6]
	push [bp+4]
	push offset trail1_matrix
	call DrawMatrix
	jmp @@exit_func
	
	@@Case2:
	push [bp+6]
	push [bp+4]
	push offset trail2_matrix
	call DrawMatrix
	jmp @@exit_func
	
	@@Case3:
	push [bp+6]
	push [bp+4]
	push offset trail3_matrix
	call DrawMatrix
	jmp @@exit_func
	
	@@Case4:
	push [bp+6]
	push [bp+4]
	push offset trail4_matrix
	call DrawMatrix
	jmp @@exit_func
		
	@@Horizontal:
			push [bp+6]
			push [bp+4]
			push offset trail_hor_matrix
			call DrawMatrix
			jmp @@exit_func	
			
	@@Vertical:
		push [bp+6]
		push [bp+4]
		push offset trail_vert_matrix
		call DrawMatrix
		
	
	@@exit_func:
	POP_ALL_BP
	ret 4
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

; Takes digits and returns an ASCII 
; INPUT: ax
; OUTPUT: ax (ASCII VALUE)
proc Num2ASCII
	add ax, 30h
	ret
endp

; DESCRIPTION: Delays the program for the inputed amount of Miliseconds
; ------------------------------------------
; SETUP:
; create a variable called timer, or dont and just erase the lines that use it
;
; HOW TO USE:
; push {amount of miliseconds}
; call TimeBasedDelay
; ------------------------------------------
proc TimeBasedDelay
	push bp 
	mov bp , sp 
    push ax
    push cx
	push dx
	mov cx, [bp+4]
	@@MyLoop:
	push cx
	mov cx, 0
	mov dx, 3D0h
	mov ah,86h
	int 15h
	inc [timer]
	pop cx
	loop @@MyLoop
	pop dx
	pop cx
	pop ax
	pop bp
	ret 2
endp TimeBasedDelay


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
; 			   given grid cell, returns a boolean through stack
; INPUT: (X, Y) -> Through stack
; OUTPUT: boolean (1 - Enabled, 0 - Disabled) -> Through stack
proc CheckWallBit
	PUSH_ALL_BP
	push [bp+6]
	push [bp+4]
	call CoordinatesToGrid
	pop bx
	mov al, WALL_BIT
	call CheckBit
	xor ah, ah
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp


; DESCRIPTION: Checks if the trail bit is enabled in 
; 			   given grid cell, returns a boolean through stack
; INPUT: (X, Y) -> Through stack
; OUTPUT: boolean (1 - Enabled, 0 - Disabled) -> Through stack
proc CheckTrailBit
	PUSH_ALL_BP
	push [bp+6]
	push [bp+4]
	call CoordinatesToGrid
	pop bx
	mov al, TRAIL_BIT
	call CheckBit
	xor ah, ah
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp


; DESCRIPTION: Checks if the outer bit is enabled in 
; 			   given grid cell, returns a boolean through stack
; INPUT: (X, Y) -> Through stack
; OUTPUT: boolean (1 - Enabled, 0 - Disabled) -> Through stack
proc CheckOuterBit
	PUSH_ALL_BP
	push [bp+6]
	push [bp+4]
	call CoordinatesToGrid
	pop bx
	mov al, OUTER_BIT
	call CheckBit
	xor ah, ah
	mov [bp+6], ax
	POP_ALL_BP
	ret 2
endp

; DESCRIPTION: Fills up the cell in which the player used to be
			 ; according to bits that are enabled in the cell
; INPUT: None
; OUTPUT: Draws cell to the screen
proc RefreshCell
	PUSH_ALL_BP
	push [bp+6]
	push [bp+4]
	call CheckWallBit
	pop ax
	cmp ax, 1
	je @@draw_wall
	
	push [playerX]
	push [playerY]
	call CheckTrailBit
	pop ax
	cmp ax, 1
	je @@draw_trail
	jmp @@draw_black

	@@draw_wall:
	push [bp+6]
	push [bp+4]
	push offset wall_matrix
	call DrawMatrix
	jmp @@exit_func
	
	@@draw_trail:
	push [bp+6]
	push [bp+4]
	call DrawTrail
	jmp @@exit_func

	@@draw_black:
	push [playerX]
	push [playerY]
	push offset black_matrix
	call DrawMatrix
	
	@@exit_func:
	POP_ALL_BP
	ret 4
endp

; ----------------------------------------------
; ---------------ENEMY FUNCTIONS----------------
; ----------------------------------------------

proc MoveEnemies
	call MoveEnemy1
	cmp [Level], 1
	je @@exit_func
	call MoveEnemy2
	cmp [Level], 2
	je @@exit_func
	call MoveEnemy3
	cmp [Level], 3
	je @@exit_func
	call MoveEnemy4
	@@exit_func:
	ret
endp

proc CreateEnemies
	call CreateEnemy1
	cmp [Level], 1
	je @@exit_func
	call CreateEnemy2
	cmp [Level], 2
	je @@exit_func
	call CreateEnemy3
	cmp [Level], 3
	je @@exit_func
	call CreateEnemy4
	@@exit_func:
	ret
endp

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
	push ENEMY1_BIT
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
	push ENEMY2_BIT
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
	push ENEMY3_BIT
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
	push ENEMY4_BIT
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


proc OpenShowBmp near
	
	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call  ShowBmp
	
	 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp

 

; input dx filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile

	
; output file dx filename to open
proc CreateBmpFile	near						 
	 
	
CreateNewFile:
	mov ah, 3Ch 
	mov cx, 0 
	int 21h
	
	jnc Success
@@ErrorAtOpen:
	mov [ErrorFile],1
	jmp @@ExitProc
	
Success:
	mov [ErrorFile],0
	mov [FileHandle], ax
@@ExitProc:
	ret
endp CreateBmpFile





proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile




; Read 54 bytes the Header
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader


proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette

proc ShowBMP 
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx,[BmpRowSize]
	
 
	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:	
	mov dx,[BmpLeft]
	
@@NextLine:
	push cx
	push dx
	
	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen
	
 
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx
	 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	
	pop dx
	pop cx
	 
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 

EndOfCsLbl:
END start
