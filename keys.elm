import Keyboard

boo = (,) <~ Keyboard.lastPressed ~ Keyboard.keysDown

main = asText <~ boo
