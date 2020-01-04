# makeKey #

```r
makeKey("test", 'de', '', NA)
makeKey('de', '')
```

# addMsg #

Adds a message and its translation(s) to the library-internal translation environment, see the 
[GNU gettext manual](https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files).

```r
options('gettext.lang'="de") # set the current language to 'de'
addMsg("Unknown system error", "Unbekannter Systemfehler")
addMsg("Unknown system error", "Error desconegut del sistema", lang="es")
addMsg("found \%d fatal error", plural="found \%d fatal errors", 
       str=c("\%d Fehler gefunden", "\%d Fehler gefunden"))
addMsg("We saw her duck", "Wir haben ihre Ente gesehen", context="duck as noun")
addMsg("We saw her duck", "Wir haben gesehen, wie sie sich duckte", context="duck as verb")
```

# addPlural #

Adds a plural definition to internal translation object. It it already exists it will be overwritten.

```r
# Czech
pl <- addPlural(text='nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;')
print(pl)
```

# bind #

Assigns a file, based on its name or a named parameter, to a language and a domain. 
The base name of a file is composed by a **domain** and **language code** 
separated by anon-letters. A language code starts with a two (ISO 639-1) 
letter code.

```r
options('gettext.lang'='en')
bind('myproject_de.po')
bind(de='myproject.po')
bind(de=c('myproject1.po', 'myproject2.po'))
bind(de=c('myproject1_de.po', 'myproject2_de.po'), 
         en=c('myproject1_en.po', 'myproject2_en.po'))
files <- c("myproject_de.po", 
           "intl/for_use_owncloud_impress_cs_CZ.po",
           "intl/ibalsa-master-zh_HK.po",
           "spirit-po/test1.po")
bind(files)
bind()
```

# bindtextdomain #

Sets the path for a domain.

```r
bindtextdomain('app2', system.file('shiny', 'app2', package='gettext'))
```

# ISO 639 language codes #

A data frame with two- and three letter codes from ISO 639-1 and ISO 693-2. 
Codes are taken from alpha-3 bibliographic, alpha-3 terminologic and alpha-2 code.

```r
data(iso639)
head(iso639)
```

# Plural forms #

A list of plural forms used in PO files.

```r
data(plural)
plural[[1]]
lang <- sapply(plural, function(v) { v$lang })
# extract arabic
ar   <- which(lang=='ar')
plural[[ar]]
# test of plural form
plural[[ar]]$plural(0:100)
```

# getTranslation #

Returns a copy of the internal translation environment or a list of `str`'s of each object.

```r
getTranslation()
```

# gettext #

Returns a translated string from the untranslated string `id1`. An abbreviated form is `G` since 
R does not support `_` as unary operator.

```r
gettext("I saw her duck") # no translation at all
G("I saw her duck") 
```

# gettextify #

Takes a file, R code or R expression and replaces all text constants by `gettext(text_constant)`. 
The replacement is not done inside `gettext`, `G`, `ngettext`, `N`, `library` and
`require`.            
If `expr` is a character then `gettextify` tries first to open a file with the name.
If this fails then it wil be assumed that `expr` contains valid R code.

```r
gettextify ('x<-"a"')
gettextify (system.file(package='gettext', 'shiny', 'app1', 'app.R'))
gettextify (parse(system.file(package='gettext', 'shiny', 'app1', 'app.R')))
```

# msgWarning #

Appends a warning to the library-internal translation environment.

```r
msgWarning('duplicate entry')
```

# ngettext #

Returns a translated string from the untranslated string `id2`. An abbreviated form is `N` since 
R does not support `_` as unary operator.

```r
ngettext("I saw one duck", "I saw \%.0f ducks", 1)
sprintf(ngettext("I saw \%0.f duck", "I saw \%.0f ducks", 3), 3) # no translation at all
sprintf(N("I saw a duck", "I saw \%.0f ducks", 3), 3)
```

# plural2R #

Converts a plural from from text to list with the input text, nplurals value and a R function 
`plural(n)` which computes the plural index from `n`. 
\itemize{
\item Note 1: The index is increased by one since R starts with x[1] and C with x[0]
\item Note 2: Only a subset of C symbols are supported based on the test cases: 
`(`, `)`, `\%`, `<`,  `>`, `>=`, `<=`, `&&`, `||`, `?:`, `=`, and `;`.
}

```r
# Belarusian plural form
p <- paste0('nplurals=3;',
     'plural=(n\%10==1 && n\%100!=11 ? 0 : n\%10>=2 && n\%10<=4 && (n\%100<10 || n\%100>=20) ? 1 : 2);')
l <- plural2R(p)
l$nplurals
l$plural(0:100)
```

# printWarnings #

Prints all warning messages for the translation object.

```r
printWarnings()
```

# readPofile #

Reads one or more PO file into the library internal translation object. 
If `file` has been created with [language()] then several files with different
domains and languages can be read at once.

```r
\dontrun{
readPofile('test_de.po', lang='de')
readPofile(languageFiles('test_de.po'))
}

```

# saveTranslation, readTranslation and cleanTranslation #

\itemize{
\item `saveTranslation` saves the library-internal translation environment as an RDS file, for details see [base::saveRDS()].
\item `readTranslation` restores the library-internal translation environment from an RDS file, for details see [base::readRDS()].
\item `cleanTranslation` deletes in all domains and languages the translations in the library-internal translation environment.
}

```r
\dontrun{
saveTranslation("translation.RDS")
cleanTranslation()
readTranslation("translation.RDS")
}
```

# writePotfile #

Creates a POT file from text extracted with [xgettext()].

```r
txt <- xgettext(parse(system.file('app1', 'app.R', package='gettext')))
\dontrun{
writePotfile(txt, 'myproject.pot')
}
```

# xgettext #

Extracts all text constants from R program including `gettext` calls.

```r
xgettext(parse(system.file('app1', 'app.R', package='gettext')))
xgettext(parse(system.file('app2', 'app.R', package='gettext')))
```

# print #

```r
txt <- xgettext(parse(system.file('shiny', 'app1', 'app.R', package='gettext')))
txt
```

