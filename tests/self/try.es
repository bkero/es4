try {
print('trying');
throw 10;
}
catch (x:int) {
print('catch ',x);
}
catch (x:*) {
print('missed ',x);
}
finally {
print('finally');
}