# FP_25-26_Inf

## Контакти

* Mail - `raya.r.litova@gmail.com`

## Ако ще работите на собствен компютър:

### Инсталиране на инструменти за работа с Haskell

Препоръчваният начин е да използвате [`ghcup`][ghcup].

На [началната страница на `ghcup`][ghcup] има едноредови инструкции за инсталирането на `ghcup`, заедно с gif, демонстриращ как да го използвате, за да инсталирате инструменти.

Алтернативно можете да разгледате [по-подробните инструкции](https://www.haskell.org/ghcup/install/) за инсталиране на `ghcup`.

```bash
ghcup install ghc 9.4.7
ghcup install cabal-install
ghcup install haskell-language-server
```

Ако сте под Linux, можете да използвате `ghcup tui` за да получите интерактивен текстов интърфейс (TUI) за работа с `ghcup`.

След това, моля проверете дали работи всичко като изпълните командата `ghci` в терминал. Би трябвало да видите нещо такова:
```
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
```

(От ghci> се излиза с команда :q)

При спънки не се безпокойте да се свържете с мен.

[ghcup]: https://www.haskell.org/ghcup/

### Редактор и интеграция с Haskell

Препоръчаният метод за работа с Haskell е [VSCode](https://code.visualstudio.com/), заедно с [HLS](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) разширението.

То включва много полезни функционалности - често срещани неща като "форматирай", "отиди на дефиниция", "какъв е типът на това", "оцени израз" и т.н.

Важно е да се отбележи, че докато работим с файл извън "проект" (което ще правим повечето време), е **нужно** да имаме инсталирано `ghc` и да е в `PATH`, за да работи HLS

### Допълнителни разширения за VSCode

[Haskell Runner 2](https://marketplace.visualstudio.com/items?itemName=Meowcolm024.runner2) - Load GHCI бутон
[haskell-linter](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter)
[Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)

[Reload](https://marketplace.visualstudio.com/items?itemName=natqe.reload) - (unrelated) Добавя бутон, който ви reload-ва VSCode вместо да го затваряте и отваряте

## Haskell ресурси (крадено от Павел Атанасов)

* [курс](https://github.com/bobatkey/CS316-2022)
* [Книгата](http://www.cs.nott.ac.uk/~pszgmh/pih.html) на която е базиран горният курс.
* [Силно препоръчително четиво][parse-dont-validate] свързано с
  * как да ни се налага да мислим по-малко докато програмираме
  * как да избягваме големи класове грешки с помощта на компилатора
  * ключова начин на мислене в Haskell
  * защо да програмираме на Haskell
* [Hoogle](https://hoogle.haskell.org/) - търсене за хаскел функции (идентификатори) (и по типове!)
* [Hackage](http://hackage.haskell.org/) - търсене за хаскел пакети
* [Real World Haskell](http://book.realworldhaskell.org/) - практично насочена

  Малко остаряла.

* Специализирани ресурси:

  * защо да правим `a -> Maybe b`, вместо `a -> Bool`:
    * [Parse, don't validate][parse-dont-validate]
    * https://runtimeverification.com/blog/code-smell-boolean-blindness/
    * https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
  * Програмиране с типове - [Thinking with Types](https://thinkingwithtypes.com/)
  * Паралелно и конкуретно програмиране - [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html)
    * една от въведителните части е доста добра за добиване на по-добро разбиране над оценителния модел на Haskell
  * Разглеждане на фундаментални типови класове - [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
  * Разглеждане на различни интересни библиотеки/разшиерния на езика - [24 days of \*](https://ocharles.org.uk/)


[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/