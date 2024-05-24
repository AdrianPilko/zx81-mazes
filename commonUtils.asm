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



setRandomNumberZeroOne

   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
	cp h
	jr z, resetRandSeed_2
	ld (randomSeed),hl
	jr endOfUpdateRandomSeed
resetRandSeed_2
    ld hl, 0
	ld (randomSeed), hl
endOfUpdateRandomSeed
	
    ld a, (hl)
    and %00000001
    ; a now contains random number zero or one
    ret

setRandomNumberFour
    ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
    inc hl
    ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
    cp h
    jr z, resetRandSeed_Four
    ld (randomSeed),hl
    jr endOfUpdateRand_Four
resetRandSeed_Four
    ld hl, 0
    ld (randomSeed), hl
endOfUpdateRand_Four

    ld a, (hl)
    and %00000011
    ; a now contains random number 0,1,2,3
    ret




; chance of getting n 1's in a row is 0.5^n
; so 0.1 = 0.5^n gives us n=(ln(0.1) / ln(0.5))
; this is about 3.32 iterations

setRandomNumberOneInTen
    ;ld b, 3
    ld b, 255 ; changed to make it less likely

NInARow
    dec b
    ld a, b
    cp 0
    jr endOfOneInTenSetAOne
   	ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
	inc hl
	ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex 
	cp h
	jr z, resetRandSeed_3
	ld (randomSeed),hl
	jr endOfUpdateRandomSeed_1
resetRandSeed_3
    ld hl, 0
	ld (randomSeed), hl
endOfUpdateRandomSeed_1
	
    ld a, (hl)
    and %00000001
    cp 1      ; try to get a 1 in ten by looping 4 times in a row if one every time
    jr z, NInARow 
    jr endOfOneInTenSetAZero
endOfOneInTenSetAOne
    ;; at this point we had enough 1's in a row
    ld a, 1
    ret
endOfOneInTenSetAZero
    ld a, 0
    ret


waitABit
	ld b,128
waitABit_WaitLoop_1
    push bc
    ld b, 222
waitABit_WaitLoop_2
    djnz waitABit_WaitLoop_2
    pop bc
	djnz waitABit_WaitLoop_1
    ret

; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
    ld hl,Display
    add hl,bc
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end
    pop de  ; preserve de
    ret

print_number16bits    ; bc stores the 16bits, print b then c, de stores offset from Display
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
    ret


print_number8bits
    ld hl, (DF_CC)
    add hl, de
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a

    ret

printNumber
    ld hl,Display
    add hl,bc
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a
    ret


;check if TV synchro (FRAMES) happend
vsync
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
endOfVsync
	ret
