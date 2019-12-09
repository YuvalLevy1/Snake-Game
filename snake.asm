	IDEAL
	MODEL small
	STACK 100h
	DATASEG
	
	ifDead dw 0						;ifDead is used to check if the snake self colided or passed the limit
	apples_location dw 0    		;apples_location hold the position of the apple
	snakeSize dw 16         		;snakes size holds the location of the last *
	firstApple dw 0					;uses to check if it is the first time the apple is being spawned
	input dw 0						;checks what is the current input from the user
	lastDirection dw 0				;holds the snake's last direction
	snakesBody dw 2000,2002,2004	;hold the location of the snake in the extra segment
	
	CODESEG 
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc WaitForData ;receive input from the user, jump to the right tag
		push bx
		push ax
		mov bx,offset input
		wfd:
		mov ah, 1
		int 16h
		jz wfd
		mov ah, 0
		int 16h
		cmp al,'w'
		jnz checka
		mov [word ptr bx],0
		mov [word ptr lastDirection],'w'
		checka:
		cmp al,'a'
		jnz checks
		mov [word ptr bx],1
		mov [word ptr lastDirection],'a'
		checks:
		cmp al,'s'
		jnz checkd
		mov [word ptr bx],2
		mov [word ptr lastDirection],'s'
		checkd:
		cmp al,'d'
		jnz checkq
		mov [word ptr bx],3
		mov [word ptr lastDirection],'d'
		checkq:
		cmp al,'q'
		jnz skipq
		mov [ifDead],111
		skipq:
		pop ax
		pop bx
		ret 
		endp WaitForData
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc clean ;erase everything on the screen
	push bp
	mov bp,sp
	push ax			
	push bx										
	push cx	
	push dx
	push di
	push si
	xor cx,cx
	xor dx,dx
	mov di,4000h
	mov al,'*'
	mov ah,0
	lop:
	sub di,2
	mov [es:di],ax
	cmp di,0
	jnz lop
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret
	endP clean
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc W 	;move the snake one pixel up
	push bp
	mov bp,sp
	push di
	push ax
	mov al,'*'
	call clean
	call moveValues
	sub [word ptr snakesBody],160
	call Amove
	pop ax
	pop di
	pop bp
	ret
	endp W
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc A ; move the snake one pixel to the left
	push bp
	mov bp,sp
	push di
	push ax
	mov al,'*'
	call clean
	call moveValues
	sub [word ptr snakesBody],2
	call Amove
	pop ax
	pop di
	pop bp
	ret
	endp A
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc S ;move the snake one pixel down 
	push bp
	mov bp,sp
	push di
	push ax
	mov al,'*'
	call clean
	call moveValues
	add [word ptr snakesBody],160
	call Amove
	pop ax
	pop di
	pop bp
	ret
	endp S
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc D ;move the snake one pixel to the right
	push bp
	mov bp,sp
	push di
	push ax
	mov al,'*'
	call clean
	call moveValues
	add [word ptr snakesBody],2
	call Amove
	pop ax
	pop di
	pop bp
	ret
	endP D
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc Amove ;print the memory
	push bp
	mov bp,sp
	push bx
	push ax
	push si
	push di
	mov al,'*'
	mov ah,02h
	mov si,[snakeSize]
	Smove:
	mov bx,[si]
	mov [es:bx],ax
	sub si,2
	cmp si,di
	ja Smove
	mov bx,[di]
	mov [es:bx],ax
	pop di
	pop si
	pop ax
	pop bx
	pop bp
	ret 
	endP Amove
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc dead ; check if the snake has left the border, if true move to byte 0 the value 111
	push bp
	mov bp,sp
	push bx
	push si
	push dx
	push ax
	xor dx,dx
	mov bx,[snakesBody]
	mov ax,[snakesBody]
	cmp [word ptr snakesBody],0
	ja skipup
	mov [word ptr ifDead],111
	skipup:
	cmp [word ptr snakesBody],4000
	jb skipdown
	mov [word ptr ifDead],111
	skipdown:
	mov bx,160
	div bx
	cmp dx,0
	jnz skipSides
	mov [word ptr ifDead],111
	skipSides:
	pop ax
	pop dx
	pop si
	pop bx
	pop bp
	ret
	endp dead
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc hitYourself ;check if the snake hit itself, if true move to byte 0 the value 111
	push bp
	mov bp,sp
	push si
	push di
	push ax
	mov si,[snakeSize]
	hitloop:
	mov ax,[di]
	cmp [si],ax
	jnz skip
	mov [word ptr ifDead],111
	jmp exitHitYourself
	skip:
	sub si,2
	cmp si,di
	ja hitloop
	exitHitYourself:
	pop ax
	pop di
	pop si
	pop bp
	ret
	endp hitYourself
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc moveValues ;changes the memory, each value is replaced with the value in the word before him
	push bp
	mov bp,sp
	push si
	push di
	push ax
	mov si,[snakeSize]
	MVswitch:
	mov ax,[si-2]
	mov [si],ax
	sub si,2
	cmp si,di
	ja MVswitch
	pop ax
	pop di 
	pop si
	pop bp
	ret
	endP moveValues
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc apple ;Spawn a random apple and check if the snake ate it, if true add to word 6 the value 2
	push bp
	mov bp,sp
	push ax
	push dx
	push cx
	push si
	mov cx,[firstApple]
	inc cx
	mov [firstApple],cx
	mov al,'@'
	mov ah,200
	cmp cx,1
	jz new
	mov si,[apples_location]
	cmp si,[snakesBody]
	jz doapple
	mov [es:si],ax
	add [word ptr firstApple],1
	pop si
	pop cx
	pop dx
	pop ax
	pop bp
	ret
	doapple:
	add [word ptr snakeSize],2
	new:
	mov ah,0h
	int 1ah
	mov dh,0
	mov ax,dx
	mov dx,14
	mul dx
	mov si,ax
	mov bx,160
	div bx
	cmp dx,0
	jz new
	mov al,'@'
	mov ah,200
	mov [es:si],ax
	mov [apples_location],si
	mov ax,0
	mov cx,24
	pop si
	pop cx
	pop dx
	pop ax
	pop bp
	ret
	endp apple
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc delay ;create a delay using loop
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	dlop:
	xor dx,dx
	inc cx
	cmp cx,0561h
	jz pend
	inflop:
	mov ax,bx
	mov bx,ax
	inc dx
	cmp dx,0411h
	jnz inflop
	jmp dlop
	pend:
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 
	endP delay
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	start: ;spawning the snake and entering the registers the required value
		mov ax, @data
		mov ds, ax
		mov ax,0B800h
		mov es,ax
		mov al,'*'
		mov ah,02h
		call clean
		mov di,offset snakesBody
		mov bx,[di]
		mov [es:bx],ax
		mov bx,[di+2]
		mov [es:bx],ax	
		mov bx,[di+4]
		mov [es:bx],ax
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mainlop:
	call WaitForData	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		moveW: 
		cmp [input], 0
		jnz moveA
		call W
		call apple
		call dead
		call hitYourself
		cmp [word ptr ifDead],111
		jz exit
		call delay
		mov ah, 1
		int 16h
		jz moveW
	;	jmp mainlop
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		moveA:
		cmp [input],1
		jnz moveS
		call A
		call apple
		call dead
		call hitYourself
		cmp [word ptr ifDead],111
		jz exit
		call delay
		mov ah, 1
		int 16h
		jz moveA
	;	jmp mainlop
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		moveS:
		cmp [input],2
		jnz moveD
		call S
		call apple
		call dead
		call hitYourself
		cmp [word ptr ifDead],111
		jz exit
		call delay
		mov ah, 1
		int 16h
		jz moveS
	;	jmp mainlop
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		moveD:
		cmp [input],3
		jnz mainlop
		call D
		call apple
		call dead
		call hitYourself
		cmp [word ptr ifDead],111
		jz exit
		call delay
		mov ah, 1
		int 16h
		jz moveD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		jmp mainlop
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		exit:
		mov ax, 4c00h
		int 21h
	END start
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;