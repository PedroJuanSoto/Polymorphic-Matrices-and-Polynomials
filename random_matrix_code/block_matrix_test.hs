module Main where
import Random_matrix_code

x = Nums (Coef [0,1]) 4

aa = x^11 + x^10 + x^9 + (x^4 + x^2)* x^2 + (x^3 + x^2 + 1)* (x^4 + 1) + 1
ab = x^12 + x^7 + (x^12 + x^2)* x^3 + (x^4 + 1)* x^2 + (x^3 + x^2 + 1)* (x^4 + x^2)
ac = x^7 + (x^4 + 1)* x^4 + (x^3 + 1)* x^3 + (x^4 + x^2)* (x^4 + x^3 + x^2 + 1)
ad = (x^4 + 1)* x^3 + x^3 + x^2 + (x^3 + x)* (x^4 + x^2) + (x^4 + 1)* (x^7 + x^3 + 1) + 1

ba = (x^3 + x^2 + 1)* x^7 + (x^9 + x^2)* x^6 + (x^3 + x^2 + 1)* x^2 + (x^4 + 1)* (x^11 + x^10 + 1)
bb = x^9 + (x^3 + x^2 + 1)^2 + (x^9 + x^2)* (x^12 + x^2) + (x^4 + 1)* (x^12 + x^7)
bc = x^11 + (x^4 + 1)* x^7 + (x^3 + x^2 + 1)* (x^4 + x^3 + x^2 + 1) + (x^3 + 1)* (x^9 + x^2)
bd = (x^7 + x^3 + 1)* x^7 + (x^3 + x)* (x^3 + x^2 + 1) + (x^3 + x^2 + 1)* (x^4 + 1) + (x^4 + 1)* (x^9 + x^2)

ca = (x^3 + x^2 + 1)* x^6 + (x^3 + x^2 + 1)* x^4 + x^4 + (x^7 + x^3 + 1)* (x^11 + x^10 + 1)
cb = x^6 + (x^3 + x^2 + 1)* x^2 + (x^3 + x^2 + 1)* (x^12 + x^2) + (x^7 + x^3 + 1)* (x^12 + x^7)
cc = x^8 + (x^7 + x^3 + 1)* x^7 + (x^4 + x^3 + x^2 + 1)* x^2 + (x^3 + 1)* (x^3 + x^2 + 1)
cd = (x^7 + x^3 + 1)* x^4 + (x^3 + x)* x^2 + (x^3 + x^2 + 1)* (x^4 + 1) + (x^3 + x^2 + 1)* (x^7 + x^3 + 1)

da = (x^3 + x^2 + 1)* x^7 + (x^11 + x^10 + 1)* x^6 + (x^12 + x^7)* x^2 + (x^3 + x^2 + 1)* (x^11 + x^10 + 1)
db = x^9 + (x^11 + x^10 + 1)* (x^12 + x^2) + 2* (x^3 + x^2 + 1)* (x^12 + x^7)
dc = x^11 + (x^3 + x^2 + 1)* x^7 + (x^3 + 1)* (x^11 + x^10 + 1) + (x^4 + x^3 + x^2 + 1)* (x^12 + x^7)
dd = (x^7 + x^3 + 1)* x^7 + (x^3 + x^2 + 1)^2 + (x^4 + 1)* (x^11 + x^10 + 1) + (x^3 + x)* (x^12 + x^7)

pa = Rows [[aa,ab],[ba,bb]]
pb = Rows [[ac,ad],[bc,bd]]
pc = Rows [[ca,cb],[da,db]]
pd = Rows [[cc,cd],[dc,dd]]

yaa = x^3
yab = x^2+x^4
yac = 1+x^4
yad = 1

yba = x^2+x^9
ybb = 1+x^2+x^3
ybc = x^7
ybd = 1+x^4

yca = 1+x^2+x^3
ycb = x^2
ycc = x^4
ycd = 1+x^3+x^7

yda = 1+x^10+x^11
ydb = x^7+x^12
ydc = x^7
ydd = 1+x^2+x^3

ya = Rows [[yaa,yab],[yba,ybb]]
yb = Rows [[yac,yad],[ybc,ybd]]
yc = Rows [[yca,ycb],[yda,ydb]]
yd = Rows [[ycc,ycd],[ydc,ydd]]

zaa = x^6
zab = x^2+x^12
zac = 1+x^3
zad = 1+x^4

zba = x^2
zbb = 1+x^2+x^3
zbc = 1+x^2+x^3+x^4
zbd = x+x^3

zca = 1+x^2+x^3
zcb = x^2
zcc = x^4
zcd = 1+x^3+x^7

zda = 1+x^10+x^11
zdb = x^7+x^12
zdc = x^7
zdd = 1+x^2+x^3

za = Rows [[zaa,zab],[zba,zbb]]
zb = Rows [[zac,zad],[zbc,zbd]]
zc = Rows [[zca,zcb],[zda,zdb]]
zd = Rows [[zcc,zcd],[zdc,zdd]]

p = Rows [[pa,pb], [pc,pd]]
y = Rows [[ya,yb], [yc,yd]]
z = Rows [[za,zb], [zc,zd]]

main = do
  let test = (p == y*z)
  print test
