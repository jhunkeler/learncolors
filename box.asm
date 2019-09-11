bits 16	   ; 16-bit mode

org 7C00h ; address of the boot sector

BootStageOne:
	cli
	mov ax, CSEG
	mov es, ax
	mov ds, ax
	sti

	mov ah, 00h	; BIOS - reset disk
	mov dl, 0	; set drive number
	int 13h		; call BIOS disk services

	mov ah, 02h	; read sectors
	mov al, 10h	; set number of sectors to read (16)
	mov dl, 0	; set drive number
	mov ch, 0	; set cylinder number
	mov dh, 0	; set head number
	mov cl, 2	; set starting sector number
	mov bx, CSEG	; set memory location to load to
	int 13h		; call BIOS disk services

	jmp CSEG:start	; "far" jump to program

CSEG equ 7E00h

EOS0:
	; pad remaining space in sector
	times ((200h - 2) - ($ - $$)) db 0
	; write boot signature
	dw 0AA55h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRAM_320x200 equ 0a000h
VRAM_80x20 equ 0b800h
VRAM_DBUF equ 0700h
VIDEO_WIDTH equ 320
VIDEO_HEIGHT equ 200
VIDEO_SIZE equ VIDEO_HEIGHT * VIDEO_WIDTH

vram_front: dw VRAM_320x200
vram_back: dw VRAM_DBUF
vram: dw VRAM_DBUF		; uninitialized video memory location

cursor_y: dw 0
cursor_x: dw 0

shape_size: dw 30h	; not too big, not too small
shape_color: dw 7		; default to gray
shape_y_coord: dw (200/2)-(30h/2)
shape_x_coord: dw (320/2)-(30h/2)
shape_xy_step: dw 4
shape_size_step: dw 4

screen_color: dw 1	; default to blue
text_color: dw 0fh	; default to gray
unbuffered_raw: db 0	; 0=process control codes, 1=do not

welcome: db 'Hi Evan! How are you today?', 0ah, 'I am good.', 0
msg_square: db 'Square', 0

start:
	; init video
	mov ax, [vram_back]
	mov [vram], ax

	push word 13h		; 320x200 VGA, 256 colors
	call set_graphics_mode

	.mainloop:
		; reset cursor position
		mov word [cursor_x], 0
		mov word [cursor_y], 0

		push word [screen_color]
		call clear_screen

		;push welcome
		;call puts

		call charset_dump

		;push word [shape_color]	 ; color
		;push word [shape_size]	 ; width
		;push word [shape_size] 	 ; height
		;push word [shape_x_coord]	 ; x
		;push word [shape_y_coord]	 ; y
		;call draw_box

		push word [shape_color]	 ; color
		push word [shape_size]	 ; width
		push word [shape_size] 	 ; height
		push word [shape_x_coord]	 ; x
		push word [shape_y_coord]	 ; y
		push word msg_square
		call draw_shape

		call doublebuffer
		call input_handler

		push word 0a00h
		push word 0000h
		call sleep		; prevent color-change induced seizures

		jmp .mainloop

	cli	; disable interrupts
	hlt	; halt CPU interrupts


doublebuffer:
	push bp
	push ax
	push bx
	push cx
	push si
	push di
	push ds
	; DS is about to change, so store it (and ES) in registers
	; ... or things will explode
	mov bx, [vram_front]
	mov ax, [vram_back]
	mov ds, ax	; set source segment
	push es
	mov ax, bx
	mov es, ax	; set destination segment

	xor si, si	; clear source index
	xor di, di	; clear destination index
	mov cx, VIDEO_SIZE/2
	repne movsw

	pop es
	pop ds
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop bp
	ret


set_cursor:
	push bp
	mov bp, sp
	push ax

	mov ax, [bp + 4]
	mov [cursor_y], ax
	mov ax, [bp + 6]
	mov [cursor_x], ax

	pop ax
	pop bp
	ret 2 * 2


puts:
	push bp
	mov bp, sp
	push ax
	push si

	mov si, [bp + 4]
	xor ax, ax

	.write:
		lodsb
		cmp ax, 0
		je .return
		call putc
		jmp .write

	.return:
	pop si
	pop ax
	pop bp
	ret 2 * 1

putc:
	push bp
	mov bp, sp
	push ax
	cmp byte [unbuffered_raw], 0
	jne .write

	cmp ax, 0ah
	je .LF

	jmp .write

	.LF:
		mov word [cursor_x], 0
		add word [cursor_y], 8
		jmp .return

	.write:
		call draw_char
		add word [cursor_x], 8
		cmp word [cursor_x], VIDEO_WIDTH
		jl .return
		mov word [cursor_x], 0
		add word [cursor_y], 8
	.return:
		pop ax
		pop bp
		ret


draw_char:
	push bp
	push ax
	push si

	mov si, font

	push word [cursor_x]
	push word [cursor_y]
	push word [text_color]
	push word ax
	push si
	call draw_bitmap

	pop si
	pop ax
	pop bp
	ret


input_handler:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	mov bx, [shape_xy_step]
	mov cx, [shape_size_step]

	mov ax, 0000h
	int 16h

	cmp al, ' '
	je .screen_color_next

	cmp al, 'x'
	je .screen_color_next

	cmp al, 'z'
	je .screen_color_prev

	cmp al, 'a'
	je .text_color_next

	cmp al, 's'
	je .text_color_prev

	cmp al, ']'
	je .shape_color_next

	cmp al, '['
	je .shape_color_prev

	cmp al, '+'
	je .shape_size_bigger

	cmp al, '-'
	je .shape_size_smaller

	cmp ah, 75		; scan-code: left arrow
	je .shape_move_left

	cmp ah, 77		; scan-code: right arrow
	je .shape_move_right

	cmp ah, 72		; scan-code: up arrow
	je .shape_move_up

	cmp ah, 80		; scan-code: down arrow
	je .shape_move_down

	jmp .return

	.screen_color_next:
		add byte [screen_color], 01h
		jmp .return

	.screen_color_prev:
		sub byte [screen_color], 01h
		jmp .return

	.text_color_next:
		add byte [text_color], 01h
		jmp .return

	.text_color_prev:
		sub byte [text_color], 01h
		jmp .return

	.shape_color_next:
		inc byte [shape_color]
		jmp .return

	.shape_color_prev:
		dec byte [shape_color]
		jmp .return

	.shape_size_bigger:
		cmp word [shape_size], 128
		je .return
		add [shape_size], cx
		jmp .return

	.shape_size_smaller:
		cmp [shape_size], cx
		jle .return
		sub [shape_size], cx
		jmp .return

	.shape_move_up:
		sub word [shape_y_coord], bx
		jmp .return

	.shape_move_down:
		add word [shape_y_coord], bx
		jmp .return

	.shape_move_left:
		sub word [shape_x_coord], bx
		jmp .return

	.shape_move_right:
		add word [shape_x_coord], bx
		jmp .return

	.return:
		pop cx
		pop bx
		pop ax
		pop bp
		ret


sleep:
	push bp
	mov bp, sp
	push ax
	push cx
	push dx

	mov ah, 86h
	mov cx, [bp + 4]
	mov dx, [bp + 6]
	int 15h

	pop dx
	pop cx
	pop ax
	pop bp
	ret 2 * 2


set_graphics_mode:
	push bp
	mov bp, sp

	push ax
	mov ah, 0
	mov al, [bp + 4]
	int 10h
	pop ax
	pop bp
	ret 2 * 1


clear_screen:
	push bp
	mov bp, sp

	push ax
	push cx
	push di
	push es
	mov ax, [vram]
	mov es, ax

	mov ax, [bp + 4]
	mov ah, al
	xor di, di
	mov cx, VIDEO_SIZE / 2
	.loop:
		stosw
		dec cx
		jne .loop

	pop es
	pop di
	pop cx
	pop ax
	mov sp, bp
	pop bp
	ret 2 * 1


draw_pixel:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push es
	mov ax, [vram]
	mov es, ax

	mov ax, [bp + 4]		; Y coord (row)
	mov bx, [bp + 6]		; X coord (col)
	mov cx, VIDEO_WIDTH
	mul cx
	add ax, bx
	cmp ax, VIDEO_SIZE
	ja .return

	mov dx, [bp + 8]		; color
	mov di, ax
	mov byte [es:di], dl
	.return:
		pop es
		pop di
		pop dx
		pop cx
		pop bx
		pop ax

		pop bp
		ret 2 * 3


draw_horizontal:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (row)
	mov bx, [bp + 6]	; X coord (col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length
	.draw:
		push dx	 ; color
		push bx	 ; X coord (col)
		push ax	 ; Y coord (row)
		call draw_pixel
		inc bx
		dec cx
		jne .draw

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_vertical:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (row)
	mov bx, [bp + 6]	; X coord (col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length
	.draw:
		push dx
		push bx
		push ax
		call draw_pixel
		inc ax
		dec cx
		jne .draw

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_shape:
	push bp
	mov bp, sp
	push ax
	push bx
	push si

	push word [bp + 14]		; color
	push word [bp + 12]		; width
	push word [bp + 10]		; height
	push word [bp + 8]		; x
	push word [bp + 6]		; y
	call draw_box

	mov si, [bp + 4]		; buffer address

	mov ax, [bp + 8]
	add ax, [bp + 10]
	sub ax, [bp + 12]
	mov bx, [bp + 6]
	add bx, [bp + 12]

	push word ax
	push word bx	; y
	call set_cursor

	push si				; write message
	call puts

	pop si
	pop bx
	pop ax
	mov sp, bp
	pop bp
	ret 2 * 6


draw_box:
	push bp
	mov bp, sp
	push ax

	mov ax, [bp + 4]	; y
	mov [.y], ax
	mov ax, [bp + 6]	; x
	mov [.x], ax
	mov ax, [bp + 8]	; height
	mov [.height], ax
	mov ax, [bp + 10]	; width
	mov [.width], ax
	mov ax, [bp + 12]	; color
	mov [.color], ax

	.draw:
		push word [.width]		; length
		push word [.color]		; color
		push word [.x]		; x
		push word [.y]		; y
		call draw_horizontal
		inc word [.y]
		dec word [.height]
		jne .draw

	pop ax
	pop bp
	ret 2 * 5
	.y: dw 0
	.x: dw 0
	.color: dw 0
	.height: dw 0
	.width: dw 0


draw_shape_orig:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (at row)
	mov bx, [bp + 6]	; X coord (at col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length

	.draw_top:
		push cx
		push dx
		push bx
		push ax
		call draw_horizontal
		dec cx
		ja .draw_top

	mov cx, [bp + 10]
	.draw_left:
		push cx
		push dx
		push bx
		push ax
		call draw_vertical
		dec cx
		ja .draw_left

	mov cx, [bp + 10]
	push ax
	add ax, cx
	.draw_bottom:
		push cx
		push dx
		push bx
		push ax
		call draw_horizontal
		dec cx
		ja .draw_bottom

	mov cx, [bp + 10]
	pop ax
	add bx, cx
	dec bx
	.draw_right:
		push cx
		push dx
		push bx
		push ax
		call draw_vertical
		dec cx
		ja .draw_right

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_triangle:
	push bp
	mov bp, sp

	nop

	pop bp
	ret

draw_bitmap:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si

	mov ax, [bp + 8]	; color
	mov word [.COLOR], ax	; (store color)
	mov si, [bp + 4] 	; address
	mov ax, [bp + 6] 	; offset
	mov bx, [bp + 10]	; y coord
	mov dx, [bp + 12]	; x coord
	shl ax, 3		; divide offset by 8
	add si, ax		; apply offset to address


	mov word [.Y], 8
	mov word [.XSTART], dx

	xor ax, ax
	.next:
		lodsb
	.row:
		mov word [.X], 0
		.col:
			rol ax, 1
			test ax, 1
			jz .nodata

			push word [.COLOR]
			push dx
			push bx
			call draw_pixel
			.nodata:
			inc dx
			inc word [.X]
			cmp word [.X], 8
			jne .col
		inc bx
		mov dx, [.XSTART]
		dec word [.Y]
		cmp word [.Y], 0
		jne .next	; jge? probably. we are missing a byte here...

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2 * 5
	.X: dw 0
	.Y: dw 0
	.COLOR dw 0
	.XSTART dw 0

charset_dump:
	push bp
	mov bp, sp
	push ax
	push cx

	mov cx, 0
	.loop:
		mov ax, cx
		call putc
		inc cx
		cmp cx, 0xff
		jne .loop
	pop cx
	pop ax
	pop bp
	ret

font:
	; Non-printable characters
	; ASCII control
	times (20h) * 8 db 00h

	; SPACE ' '
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; !
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00000000b
	db 00011000b
	db 00000000b
	; "
	db 01100110b
	db 01100110b
	db 01100110b
	db 00100100b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; #
	db 00100100b
	db 01111110b
	db 00100100b
	db 00100100b
	db 00100100b
	db 01111110b
	db 00100100b
	db 00000000b
	; $
	db 00010000b
	db 00111110b
	db 01010000b
	db 00111100b
	db 00010010b
	db 01111100b
	db 00010000b
	db 00000000b
	; %
	db 00000000b
	db 01100010b
	db 01100100b
	db 00001000b
	db 00010000b
	db 00100110b
	db 01000110b
	db 00000000b
	; &
	db 00011000b
	db 00100100b
	db 00101000b
	db 00011010b
	db 00100100b
	db 00111110b
	db 00000000b
	db 00000000b
	; '
	db 00011000b
	db 00011000b
	db 00011000b
	db 00001000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	;db 00000000b ;<< wtf?
	; (
	db 00001000b
	db 00010000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00010000b
	db 00001000b
	db 00000000b
	; )
	db 00001000b
	db 00000100b
	db 00000010b
	db 00000010b
	db 00000010b
	db 00000100b
	db 00001000b
	db 00000000b
	; *
	db 00000000b
	db 00100010b
	db 00010100b
	db 01111111b
	db 00010100b
	db 00100010b
	db 00000000b
	db 00000000b
	; +
	db 00000000b
	db 00001000b
	db 00001000b
	db 01111111b
	db 00001000b
	db 00001000b
	db 00000000b
	db 00000000b
	; ,
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00001100b
	db 00001100b
	db 00011000b
	db 00000000b
	; -
	db 00000000b
	db 00000000b
	db 00000000b
	db 01111110b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; .
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00011000b
	db 00011000b
	db 00000000b
	; /
	db 00000001b
	db 00000010b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 01000000b
	db 00000000b
	; 0
	db 00011100b
	db 00100010b
	db 01000101b
	db 01001001b
	db 01010001b
	db 00100010b
	db 00011100b
	db 00000000b
	; 1
	db 00001000b
	db 00011000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00111110b
	db 00000000b
	; 2
	db 00111100b
	db 01000010b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 01111110b
	db 00000000b
	; 3
	db 00111100b
	db 01000010b
	db 00000010b
	db 00001100b
	db 00000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; 4
	db 00001000b
	db 00011000b
	db 00101000b
	db 01001000b
	db 01111110b
	db 00001000b
	db 00001000b
	db 00000000b
	; 5
	db 01111110b
	db 01000000b
	db 01000000b
	db 01111100b
	db 00000010b
	db 00000010b
	db 01111100b
	db 00000000b
	; 6
	db 00111110b
	db 01000000b
	db 01000000b
	db 01111100b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; 7
	db 01111110b
	db 00000010b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 01000000b
	db 00000000b
	; 8
	db 00111100b
	db 01000010b
	db 01000010b
	db 00111100b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; 9
	db 00111100b
	db 01000010b
	db 01000010b
	db 00111110b
	db 00000010b
	db 00000010b
	db 00000010b
	db 00000000b
	; :
	db 00000000b
	db 00011000b
	db 00011000b
	db 00000000b
	db 00011000b
	db 00011000b
	db 00000000b
	db 00000000b
	; ;
	db 00000000b
	db 00011000b
	db 00011000b
	db 00000000b
	db 00011000b
	db 00010000b
	db 00000000b
	db 00000000b
	; <
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 00010000b
	db 00001000b
	db 00000100b
	db 00000000b
	; =
	db 00000000b
	db 00000000b
	db 01111110b
	db 00000000b
	db 01111110b
	db 00000000b
	db 00000000b
	db 00000000b
	; >
	db 00100000b
	db 00010000b
	db 00001000b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 00000000b
	; ?
	db 00111100b
	db 01000010b
	db 00000010b
	db 00011100b
	db 00010000b
	db 00000000b
	db 00010000b
	db 00000000b
	; @
	db 00011100b
	db 00100010b
	db 01000010b
	db 01011110b
	db 01001110b
	db 01000000b
	db 00111110b
	db 00000000b
	; A
	db 00111100b
	db 01000010b
	db 01000010b
	db 01111110b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00000000b
	; B
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 00000000b
	; C
	db 00111100b
	db 01000010b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000010b
	db 00111100b
	db 00000000b
	; D
	db 01111100b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01111100b
	db 00000000b
	; E
	db 01111110b
	db 01000000b
	db 01000000b
	db 01111000b
	db 01000000b
	db 01000000b
	db 01111110b
	db 00000000b
	; F
	db 01111110b
	db 01000000b
	db 01000000b
	db 01111000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00000000b
	; G
	db 00111100b
	db 01000010b
	db 01000000b
	db 01001110b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; H
	db 01000001b
	db 01000001b
	db 01000001b
	db 01111111b
	db 01000001b
	db 01000001b
	db 01000001b
	db 00000000b
	; I
	db 01111111b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 01111111b
	db 00000000b
	; J
	db 00001110b
	db 00000010b
	db 00000010b
	db 00000010b
	db 00000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; K
	db 01000010b
	db 01000100b
	db 01001000b
	db 01110000b
	db 01001000b
	db 01000100b
	db 01000010b
	db 00000000b
	; L
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01111110b
	db 00000000b
	; M
	db 01000010b
	db 01100110b
	db 01011010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00000000b
	; N
	db 01000010b
	db 01100010b
	db 01010010b
	db 01001010b
	db 01000110b
	db 01000010b
	db 01000010b
	db 00000000b
	; O
	db 00111100b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; P
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00000000b
	; Q
	db 00111100b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01001010b
	db 01000110b
	db 00111100b
	db 00000000b
	; R
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 01001000b
	db 01000100b
	db 01000010b
	db 00000000b
	; S
	db 00111110b
	db 01000000b
	db 01000000b
	db 00111100b
	db 00000010b
	db 00000010b
	db 01111100b
	db 00000000b
	; T
	db 01111111b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00000000b
	; U
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; V
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00100100b
	db 00011000b
	db 00000000b
	; W
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01001010b
	db 01011010b
	db 01100110b
	db 00000000b
	; X
	db 10000010b
	db 01000100b
	db 00101000b
	db 00010000b
	db 00101000b
	db 01000100b
	db 10000010b
	db 00000000b
	; Y
	db 10000010b
	db 01000100b
	db 00101000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00000000b
	; Z
	db 11111110b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 01000000b
	db 11111110b
	db 00000000b
	; [
	db 00111100b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00111100b
	db 00000000b
	; \
	db 10000000b
	db 01000000b
	db 00100000b
	db 00010000b
	db 00001000b
	db 00000100b
	db 00000010b
	db 00000000b

	db 00000000b	; where does this extra byte keep coming from?

	; ]
	db 00111100b
	db 00000100b
	db 00000100b
	db 00000100b
	db 00000100b
	db 00000100b
	db 00111100b
	db 00000000b
	; ^
	db 00001000b
	db 00010100b
	db 00100010b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; _
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 01111110b
	db 00000000b
	; `
	db 00010000b
	db 00001000b
	db 00000100b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; a
	db 00000000b
	db 00000000b
	db 00111100b
	db 00000010b
	db 00111110b
	db 01000010b
	db 00111110b
	db 00000000b
	; b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 00000000b
	; c
	db 00000000b
	db 00000000b
	db 00000000b
	db 00111110b
	db 01000000b
	db 01000000b
	db 00111110b
	db 00000000b
	; d
	db 00000010b
	db 00000010b
	db 00000010b
	db 00111110b
	db 01000010b
	db 01000010b
	db 00111110b
	db 00000000b
	; e
	db 00000000b
	db 00000000b
	db 00111100b
	db 01000010b
	db 01111100b
	db 01000000b
	db 00111110b
	db 00000000b
	; f
	db 00000000b
	db 00011100b
	db 00100010b
	db 00100000b
	db 01111000b
	db 00100000b
	db 00100000b
	db 00000000b
	; g
	db 00000000b
	db 00000000b
	db 00111100b
	db 01000010b
	db 00111110b
	db 00000010b
	db 01111100b
	db 00000000b
	; h
	db 01000000b
	db 01000000b
	db 01000000b
	db 01111100b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00000000b
	; i
	db 00000000b
	db 00010000b
	db 00000000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00000000b
	; j
	db 00000000b
	db 00000010b
	db 00000000b
	db 00000010b
	db 00000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; k
	db 00100000b
	db 00100000b
	db 00100000b
	db 00100100b
	db 00101000b
	db 00111000b
	db 00100100b
	db 00000000b
	; l
	db 01100000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00100000b
	db 00110000b
	db 00000000b
	; m
	db 00000000b
	db 00000000b
	db 01010100b
	db 01101010b
	db 01001010b
	db 01000010b
	db 01000010b
	db 00000000b
	; n
	db 00000000b
	db 00000000b
	db 01011100b
	db 01100010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00000000b
	; o
	db 00000000b
	db 00000000b
	db 00111100b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; p
	db 00000000b
	db 00000000b
	db 01111100b
	db 01000010b
	db 01000010b
	db 01111100b
	db 01000000b
	db 01000000b
	; q
	db 00000000b
	db 00000000b
	db 00111110b
	db 01000010b
	db 01000010b
	db 00111110b
	db 00000010b
	db 00000010b
	; r
	db 00000000b
	db 00000000b
	db 01011100b
	db 01100010b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00000000b
	; s
	db 00000000b
	db 00000000b
	db 00111110b
	db 01000000b
	db 00111100b
	db 00000010b
	db 01111100b
	db 00000000b
	; t
	db 00000000b
	db 00010000b
	db 00010000b
	db 01111100b
	db 00010000b
	db 00010000b
	db 00011100b
	db 00000000b
	; u
	db 00000000b
	db 00000000b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00111100b
	db 00000000b
	; v
	db 00000000b
	db 00000000b
	db 01000010b
	db 01000010b
	db 01000010b
	db 00100100b
	db 00011000b
	db 00000000b
	; w
	db 00000000b
	db 00000000b
	db 01000010b
	db 01000010b
	db 01000010b
	db 01011010b
	db 01100110b
	db 00000000b
	; x
	db 00000000b
	db 00000000b
	db 01000010b
	db 00100100b
	db 00011000b
	db 00100100b
	db 01000010b
	db 00000000b
	; y
	db 00000000b
	db 00000000b
	db 01000010b
	db 00100100b
	db 00011000b
	db 00010000b
	db 00100000b
	db 00000000b
	; z
	db 00000000b
	db 00000000b
	db 01111100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 01111100b
	db 00000000b
	; {
	db 00001000b
	db 00010000b
	db 00010000b
	db 00100000b
	db 00010000b
	db 00010000b
	db 00001000b
	db 00000000b
	; |
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00010000b
	db 00000000b
	; }
	db 00100000b
	db 00010000b
	db 00010000b
	db 00001000b
	db 00010000b
	db 00010000b
	db 00100000b
	db 00000000b
	; ~
	db 00000000b
	db 00000000b
	db 00000000b
	db 01100100b
	db 10011000b
	db 00000000b
	db 00000000b
	db 00000000b
	; DEL
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b

	; Extended characters (incomplete)
	times 80h * 8 db 0ffh

; airplane
airplane:
	db 00000000b
	db 00000000b
	db 00001111b
	db 00010000b
	db 00011111b
	db 00000000b
	db 00000000b
	db 00000000b

	db 00000000b
	db 00000000b
	db 11111111b
	db 00000111b
	db 11111111b
	db 00000011b
	db 00000001b
	db 00000000b

	db 00000000b
	db 00000000b
	db 11111111b
	db 11110000b
	db 11110111b
	db 11110000b
	db 11110000b
	db 11110000b

	db 00111110b
	db 01000010b
	db 10000010b
	db 00111110b
	db 11111110b
	db 00011100b
	db 00001100b
	db 00000000b
