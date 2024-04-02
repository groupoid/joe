Прискорений курс MinCaml компілятора
====================================
Ейдзіро Суміі, Університет Токіо

Зміст
-----

* Що це?
* Makefile проєкту MinCaml
* Головна програма
* Синтаксис і типи MinCaml
* Політика компіляції
* Лексичний аналіз
* Парсер
* Вивід типів
* K-нормалізація
* α-конверсія
* β-редукція
* Редукція вкладених let
* Inline експансія
* Згортка констант
* Елімінація необов'язкових дефініцій
* Closure Conversion
* Генерація байткоду і компіляція бінарного коду
* Оптимізація безпосередніх операндів
* Алокація регістрів
* Генерація асемблерного коду

Що це?
------

Цей документ є підручником з описом MinCaml,
компілятора підмножини мови програмування ML.
Компілятор MinCaml розроблено в дослідницькому
програмному проекті «Красивий компілятор ML у Японії»
з метою популяризації простої та потужної мови програмування ML
і розриву негативної спіралі «Я не використовую ML,
тому що я не знаю ML». Він реалізований на мета мові
ML під назвою Objective Caml. Оскільки цей підручник
передбачає певний досвід програмування ML як передумову,
будь ласка, спочатку подивіться «Programmin in ML» Роберта Харпера.

[Застереження: цей підручник англійською мовою є попереднім
перекладом японської версії та все ще може мати багато недоліків,
як технічно, так і лінгвістично. Академічна стаття більш повна.]

[Оновлення від 17 вересня 2008 р.: тепер підтримується
PowerPC (на додаток до SPARC), завдяки пані Масуко та
професору Асаї з університету Очаномідзу. Ви повинні
виконати ./to_ppc або ./to_sparc перед make.]

[Ще одне оновлення від 17 вересня 2008 р.: розподільник
регістрів тепер використовує простіший алгоритм. У попередніх
версіях відсутнє зворотне відстеження (ToSpill і NoSpill).]

[Оновлення від 27 серпня 2012 р.: тепер підтримується
x86 із SSE2 (Pentium 4 або новіша версія). Будь ласка,
виконайте ./to_x86 перед make.]