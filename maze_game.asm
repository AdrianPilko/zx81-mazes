; Copyright (c) 2024 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;;; Maze game
;;;
;;; https://youtube.com/@byteforever7829

;;; Known bug(s)

;pasmo only accepts DEFINE


CLS EQU $0A2A
PRINTAT EQU $08f5
PRINT EQU $10


KEYBOARD_READ_PORT_P_TO_Y	EQU $DF
; for start key
KEYBOARD_READ_PORT_A_TO_G	EQU $FD
; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V EQU $FE
; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B EQU $7F
; keyboard q to t
KEYBOARD_READ_PORT_Q_TO_T EQU $FB

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT EQU $FE
SCREEN_WIDTH EQU 32
SCREEN_HEIGHT EQU 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
MISSILE_COUNTDOWN_INIT EQU 18
;#define PLAYER_START_POS 604
PLAYER_START_POS EQU 637
PLAYER_LIVES EQU 3
LEVEL_COUNT_DOWN_INIT EQU 4
LEV_COUNTDOWN_TO_INVOKE_BOSS EQU 1

VSYNCLOOP       EQU      3

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt
_SD:			EQU	$0D	;$
_DOLLAR:        EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F
_INV_QUOTES                     EQU $8B
_INV_A          EQU $A6

;;;; this is the whole ZX81 runtime system and gets assembled and
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM


start
    call CLS

    ld bc, 105
    ld de, START_GAME_TITLE
    call printstring

   
    ld bc, 339
    ld de, START_TEXT1
    call printstring

    ld bc, 204
    ld de, START_TEXT2
    call printstring

    ld bc, 432
    ld de, START_TEXT3
    call printstring

    ld bc, 498
    ld de, START_TEXT4
    call printstring

    ld bc, 562
    ld de, START_GAME_CRED1
    call printstring


    ld bc, 630
    ld de, START_GAME_CRED2
    call printstring
          
    xor a
    ld a, (score_mem_tens)
    ld a, (score_mem_hund)

introWaitLoop
	ld b,64
introWaitLoop_1
    push bc
    pop bc
	djnz introWaitLoop_1

   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex
	cp h
	jr z, resetRandSeed_1
	ld (randomSeed),hl
	jp read_start_key
resetRandSeed_1
    ld hl, 0
	ld (randomSeed), hl
	jp read_start_key     ;; have to have 2 labels as not a call return

read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop

    jr preinit  ; not really necessary

preinit
    ld a, 20
    ld (genRow), a
    ld a, 1
    ld (genCol), a

	;call CLS  ; clears screen and sets the boarder
    ld bc, 1
    ld de, MAZE_TEXT
    call printstring
    call fillScreenBlack
    call generateMaze
    xor a
    ld (enemyAddedFlag),a
    
    ld de, 661
    ld hl, Display+1
    add hl, de
    ld a, _INV_A
    ld (hl),a
    ld (playerAbsAddress),hl

    ld de, 62   ; exit location
    ld hl, Display+1
    add hl, de
    ld a, 8
    ld (hl),a
    

gameLoop
    ld b,VSYNCLOOP
waitForTVSync
    call vsync
    djnz waitForTVSync

    ld hl, (playerAbsAddress)
    ld a, _INV_A
    ld (hl),a
    
    push hl
       call updateEnemies
    
       ld hl, (enemyLocation)
       ld a,_INV_QUOTES
       ld (hl), a
       call printLivesAndScore
    pop hl

; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7
; in a, $fe
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd

    ;; read keys
    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a                            ; O
    jp z, moveLeft

    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a					        ; P
    jp z, moveRight

    ld a, KEYBOARD_READ_PORT_Q_TO_T
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; Q
    jp z, moveUp

    ld a, KEYBOARD_READ_PORT_A_TO_G
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; A
    jp z, moveDown
	jp gameLoop
moveLeft
	ld (prevPlayerAddress),hl
    ld hl, (playerAbsAddress)
	dec hl
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, gameLoop
	cp 8 ; exit found
	jp z, gameWon
	cp _DOLLAR
	push hl
	call z, increaseScore
	pop hl
	ld (playerAbsAddress), hl
	ld hl,(prevPlayerAddress)
	ld (hl), _DT
	jp gameLoop
moveRight
    ld (prevPlayerAddress),hl
    ld hl, (playerAbsAddress)
	inc hl
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, gameLoop
	cp 8 ; exit found
	jp z, gameWon
	cp _DOLLAR
	push hl
	call z, increaseScore
	pop hl
	ld (playerAbsAddress), hl
	ld hl,(prevPlayerAddress)
	ld (hl), _DT
	jp gameLoop
moveUp
    ld (prevPlayerAddress),hl
    ld hl, (playerAbsAddress)
	ld de, -33
	add hl, de
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, gameLoop
	cp 8 ; exit found
	jp z, gameWon
	cp _DOLLAR
	push hl
	call z, increaseScore
	pop hl
	ld (playerAbsAddress), hl
	ld hl,(prevPlayerAddress)
	ld (hl), _DT
	jp gameLoop
moveDown
    ld (prevPlayerAddress),hl
    ld hl, (playerAbsAddress)
	ld de, 33
	add hl, de
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, gameLoop
	cp 8 ; exit found
	jp z, gameWon
	cp _DOLLAR
	push hl
	call z, increaseScore
	pop hl
	ld (playerAbsAddress), hl
	ld hl,(prevPlayerAddress)
	ld (hl), _DT
    jp gameLoop
gameWon
    call increaseScore
    ld bc, 308
    ld de, YOU_WON_TEXT_0
    call printstring
    ld bc, 341
    ld de, YOU_WON_TEXT_1
    call printstring
    ld bc, 374
    ld de, YOU_WON_TEXT_2
    call printstring

    call waitABit
    call waitABit
    call waitABit
    call waitABit
    jp preinit
    ret

gameOver
    ld bc, 308
    ld de, YOU_WON_TEXT_0 ;; reuse boarder
    call printstring
    ld bc, 341
    ld de, YOU_LOST_TEXT_1
    call printstring
    ld bc, 374
    ld de, YOU_WON_TEXT_2
    call printstring

    call waitABit
    call waitABit
    call waitABit
    call waitABit
    pop bc ; maintain stack integrity as previous call no return
    jp start


generateMaze
;; uses binary tree algorithm to generate a maze
genLoop
    ;; set the current location to blank
    ld a, (genCol)
    ld c, a
    ld a, (genRow)	 ; col set for PRINTAT
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    ;generate a random number zero or one
    ld hl, (randomSeed)
    ld a, (genRow)
    cp 2
    call z, setRightCellZero
    ld a, (genRow)
    cp 2
    jp z, checkGenColRow
    call setRandomNumberZeroOne
    ld (randomSeed), hl
    cp 0
    jp z, setBlankAbove
    ;; else the column to the right blank
    ld a, (genCol)
    inc a
    ld c, a
    ld a, (genRow)
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    jr checkGenColRow

setBlankAbove
    call setCellAboveBlank

    ; in this case sometimes set the row above that to blank (to avoid having closed loops)
    ;; only do this if we're 3 away from top row
    call setRandomNumberZeroOne
    cp 1
    jr z, checkGenColRow

;; randomly add asterixes to collect
    ld a,(genRow)
    ld b, a
    ld a, (genCol)	 ; col set for PRINTAT
    ld c, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,_DOLLAR
    call PRINT
    ld a, (enemyAddedFlag)
    cp 1
    jp z, checkGenColRow
    ;; now randomly set enemies based on a 1/10 chance of the money set
    call setRandomNumberOneInTen
    cp 1
    jp z, setEnemyPosition_1
    jr checkGenColRow
setEnemyPosition_1 ; compound 3 calls
    call setRandomNumberOneInTen
    cp 1
    jp z, setEnemyPosition_2
    jr checkGenColRow    
setEnemyPosition_2
    call setRandomNumberOneInTen
    cp 1
    jp z, setEnemyPosition_3
    jr checkGenColRow    
setEnemyPosition_3
    ;; since we're (for other reasons) not using a memory address to set the maze
    ;; we need to convert from row column to screen memory
    
    ld hl, Display+1
    ld a,(genRow)
    ld b, a
    ld de, 33
enemyAddLoop
    add hl, de
    djnz enemyAddLoop
    ld de, 0
    ld e, a
    add hl, de
    ld (enemyLocation), hl
    ld a, _AS
    ld (hl), a
    ld a,1
    ld (enemyAddedFlag),a

checkGenColRow
    ld a, (genCol)
    inc a          ; skip to next column
    inc a
    ld (genCol), a ; store the next column on

;; we want to set the current location to blank
    ld a, (genCol)
    ld c, a
    ld a, (genRow)	 ; col set for PRINTAT
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT
    ld a, (genCol)  ; store the next column on
    cp 29
    call z, setCellAboveBlank ; here we have to set the wall above to blank
    ld a, (genCol)  ; store the next column on
    cp 29
    jr z, resetGenColAndDec
    cp 30
    jr z, resetGenColAndDec
    cp 31
    jr z, resetGenColAndDec
    ld (genCol),a
    jp genLoop
resetGenColAndDec
    ld a, 1
    ld (genCol),a
    ld a,(genRow)
    dec a
    dec a    ; jump 2 row up as we only set as if a grid not every row
    cp 0
    jr z, endOfGen
    cp 1
    jr z, endOfGen
    ld (genRow),a
    jp genLoop
endOfGen

    ; now set the entry point to the maze row 21
    ;; we want to set the current location to blank
    ;ld a, 0
    ;ld c, a  ; col for PRINTAT
    ;ld a, 20 ; row set for PRINTAT
    ;ld b, a
    ;call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ;ld a,0
    ;call PRINT
	ret


setCellAboveBlank
    ld a,(genRow)
    dec a
    ld b, a
    ld a, (genCol)	 ; col set for PRINTAT
    ld c, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT
    ret
setRightCellZero
    ld a,(genRow)
    ld b, a
    ld a, (genCol)	 ; col set for PRINTAT
    inc a
    ld c, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT
    ret


fillScreenBlack
    ld hl,Display+1
    ld de, 33
    add hl, de
    ld a, 128
    ld b, 21
rowLoop
    push bc
    ld b, 32
colLoop
    ld (hl),a
    inc hl
    djnz colLoop
    inc hl
    pop bc
    djnz rowLoop
    ret


increaseScore
    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa                                   ; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_hund), a
skipAddHund
	ret

printLivesAndScore
    ;ld a, (playerLives)
    ;ld de, 29
    ;call print_number8bits

    ld bc, 31
    ld de, score_mem_tens
    call printNumber

    ld bc, 29
    ld de, score_mem_hund
    call printNumber

    ;ld a, (gameLevel)
    ;ld de, 20
    ;call print_number8bits

    ret

copyFromScrBuffToDisplayMem
	ld b, 21
	ld hl, mazeScreenBuffer  ; has to be 32 * 21
	ld de, Display+34
copyScrBuffLoop
    push bc
    ld bc, 32
    ldir
    inc de ; inc de to next line because of $76 at end of line
    pop bc
    djnz copyScrBuffLoop
    ret
    
    
updateEnemies
;; select a random new locaiton of the enemy
    ld hl, (enemyLocation)
    ld (prevEnemyLocation),hl
   
    ld (hl), 0
    ;ld a, 8
    ;ld (hl),a
    push hl
    call setRandomNumberFour
    pop hl
    cp 0
    jr z, moveEnLeft
    cp 1
    jr z, moveEnRight
    cp 2
    jr z, moveEnUp
    cp 3
    jr z, moveEnDown
    jp endOfUpdateEnemy
        
    
moveEnLeft
	dec hl
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, moveRightAndInc ; if it isn't possible to move try different move untill it is!
	cp 8
	jp z, moveRightAndInc ; if it isn't possible to move try different move untill it is!
	cp _INV_A ; game over
	jp z, gameOver
	ld (enemyLocation), hl
	ld hl,(prevEnemyLocation)
	ld (hl), 0
	ret
	jp endOfUpdateEnemy
moveRightAndInc
    inc hl   ; this undoes the inc before to maintain consistency
moveEnRight
	inc hl
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, moveUpAndDec ; if it isn't possible to move try different move untill it is!
	cp 8
	jp z, moveUpAndDec ; if it isn't possible to move try different move untill it is!
	cp _INV_A ; game over
	jp z, gameOver
	ld (enemyLocation), hl
	ld hl,(prevEnemyLocation)
	ld (hl), 0
	jp endOfUpdateEnemy
moveUpAndDec
    dec hl   ; this unodes the previous dec before to maintain consistency
moveEnUp
	ld de, -33
	add hl, de
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, moveDownAndAdd ; if it isn't possible to move try different move untill it is!
	cp 8
	jp z, moveDownAndAdd ; if it isn't possible to move try different move untill it is!
	cp _INV_A ; game over
	jp z, gameOver
	ld (enemyLocation), hl
	ld hl,(prevEnemyLocation)
	ld (hl), 0
	jp endOfUpdateEnemy
moveDownAndAdd
	ld de, 33
	add hl, de
moveEnDown
	ld de, 33
	add hl, de
	ld a, (hl)  ; check not a wall
	cp 128
	jp z, endOfUpdateEnemy ; if it isn't possible to move try different move untill it is!
	cp 8
	jp z, endOfUpdateEnemy ; if it isn't possible to move try different move untill it is!
	cp _INV_A ; game over
	jp z, gameOver
	ld (enemyLocation), hl
	ld hl,(prevEnemyLocation)
	ld (hl), 0
	jp endOfUpdateEnemy

endOfUpdateEnemy
    ret



INCLUDE commonUtils.asm

                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76

Variables
score_mem_tens
    DB 0
score_mem_hund
    DB 0
prevPlayerAddress
    DW 0
playerAbsAddress
    DW 0
enemyLocation
    DW 0
prevEnemyLocation
    DW 0
enemyAddedFlag
    DB 0
genCol
    DB 0
genRow
    DB 0
mazeVisitedLocations
    DS 32*21, 0
mazeScreenBuffer
    DS 32*21, 8
randomSeed
    DW 0
YOU_WON_TEXT_0
    DB 7,3,3,3,3,3,3,3,3,3,3,3,3,132,$ff
YOU_LOST_TEXT_1
    DB 5,_Y,_O,_U,__,_L,_O,_S,_E,_CL,_MI,_OP,__,133,$ff
YOU_WON_TEXT_1
    DB 5,_Y,_O,_U,__,_E,_S,_C,_A,_P,_E,_D,_QM,133,$ff
YOU_WON_TEXT_2
    DB 130,131,131,131,131,131,131,131,131,131,131,131,131,129,$ff

MAZE_TEXT
    DB _M,_A,_Z,_E,_CL,0,0,0,0,0,0,0,0,0,0,0,0,0,_S,_C,_O,_R,_E,_CL,$FF
START_GAME_TITLE
    DB 	139,139,139,139,139,0,_Z,_X,_8,_1,0,_M,_A,_Z,_E,_S,0,139,139,139,139,139,$ff
START_GAME_CRED1
   DB  139, 0, 139,0,0,0, _Y,_O,_U,_T,_U,_B,_E,_CL,0,_B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,0,0,0,139, 0, 139,$ff
START_GAME_CRED2  
    DB 139,139,139,139,139,139,0,0,_V,_E,_R,_S,_I,_O,_N,0,_V,_0,_DT,_5,0,0,139,139,139,139,139,139,$ff
START_TEXT1
    DB _P, _R, _E, _S, _S, 0, _S,0, _T, _O, 0, _S, _T, _A, _R, _T,$ff
START_TEXT2
    DB _E, _N, _T, _E, _R, _CM,0, _A, _N, 0, _E, _N, _D, _L, _E, _S, _S, 0, _M, _A, _Z, _E,$ff
START_TEXT3
    DB _K, _E, _Y, _S,_CL, $ff
START_TEXT4
    DB _Q, 0, _U, _P, 0, _A, 0, _D, _O, _W, _N, 0, _O, 0, _L, _E, _F,_T,0,_P,0,_R,_I,_G,_H,_T,$ff
VariablesEnd:   DB $80
BasicEnd:


