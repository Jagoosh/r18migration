# Возрастная миграция населения и кадровый потенциал муниципальных образований УР: Модель и данные.

### Файл article.R

Код формирующий семейство моделей, а также рисунки и таблицы статьи.

### Файл data/DB_population.csv

Численность постоянного населения Удмуртской Республики
на 1 января указанного (2012—2016) года:

- год измерения (n_year);
- административно-территориальная единица (area);
- возраст^1^, полных лет (age);
- пол (gender);
- численность возрастно-половой группы (value).

^1^ — указанный возраст соответствует году, предшествующему указанному в `n_year`; значение `age == 100` в таблице соответствует «100 полных лет и старше».

Источник данных: [Удмуртстат, официальная статистика, население — основные показатели](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/ru/statistics/population/), подготовлено к анализу автором.

- [Возрастно-половой состав населения МО УР на 1 января 2012](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/953576804f382b67b844fa3a99b5ae2d/%D0%92%D0%BE%D0%B7%D1%80%D0%B0%D1%81%D1%82%D0%BD%D0%BE-%D0%BF%D0%BE%D0%BB%D0%BE%D0%B2%D0%BE%D0%B9+%D1%81%D0%BE%D1%81%D1%82%D0%B0%D0%B2+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%B3%D0%BE%D1%80%D0%BE%D0%B4%D0%BE%D0%B2+%D0%B8+%D1%80%D0%B0%D0%B9%D0%BE%D0%BD%D0%BE%D0%B2+%D0%A3%D0%A0+%D0%BD%D0%B0+1+%D1%8F%D0%BD%D0%B2%D0%B0%D1%80%D1%8F+2012.zip);
- [Возрастно-половой состав населения МО УР на 1 января 2013](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/4fce280043a5975aae59bed06954faf7/%D0%92%D0%BE%D0%B7%D1%80%D0%B0%D1%81%D1%82%D0%BD%D0%BE-%D0%BF%D0%BE%D0%BB%D0%BE%D0%B2%D0%BE%D0%B9+%D1%81%D0%BE%D1%81%D1%82%D0%B0%D0%B2+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%9C%D0%9E+%D0%A3%D0%A0+%D0%BD%D0%B0+1+%D1%8F%D0%BD%D0%B2%D0%B0%D1%80%D1%8F+2013.zip);
- [Возрастно-половой состав населения МО УР на 1 января 2014](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/c586db8047ddeb449b85bfed3bc4492f/%D0%92%D0%BE%D0%B7%D1%80_%D0%9C%D0%9E_2014.pdf);
- [Возрастно-половой состав населения МО УР на 1 января 2015](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/ee05e1004b5aa20db9cdb94e4d05559c/%D0%92%D0%BE%D0%B7%D1%80_%D0%9C%D0%9E_2015.pdf);
- [Возрастно-половой состав населения МО УР на 1 января 2016](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/19c45b804f7e1ac09445d68250d62a05/%D0%92%D0%BE%D0%B7%D1%80_%D0%9C%D0%9E_2016.pdf).

### Файл data/DB_events.csv

Движение населения в Удмуртской Республике в указанном (1990—2015) году:

- год измерения (year);
- возрастная группа (age);
- административно-территориальная единица (area);
- пол (gender);
- тип события (event):
+ родившихся (born),
+ прибывших (arrive),
+ выбывших (depart),
+ умерших (died);
- численность группы населения (value).

Источник данных: [Удмуртстат, официальная статистика, население — основные показатели](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/ru/statistics/population/), подготовлено к анализу автором.

- [Естественное движение населения УР за 2012 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/d7c94c004fc853378524f76be9e332ec/%D0%A1%D0%BC%D0%B5%D1%80%D1%82%D0%BD%D0%BE%D1%81%D1%82%D1%8C+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%BF%D0%BE+%D0%BF%D1%80%D0%B8%D1%87%D0%B8%D0%BD%D0%B0%D0%BC+%D1%81%D0%BC%D0%B5%D1%80%D1%82%D0%B8+%D0%9C%D0%9E+%D0%A3%D0%A0+2012.zip);
- [Естественное движение населения УР за 2013 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/066c0e80439251a391a095d06954faf7/%D0%95%D1%81%D1%82%D0%B5%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D0%BE%D0%B5+%D0%B4%D0%B2%D0%B8%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%A3%D0%A0+%D0%B7%D0%B0+2013.zip);
- [Естественное движение населения УР за 2014 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/900bf800486db733a556f5f7eaa5adf2/%D0%95%D0%94%D0%9D_2014.pdf);
- [Естественное движение населения УР за 2015 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/235814004cbefd42a0a4f54fc772e0bb/%D0%95%D0%94%D0%9D_2015.rar);
- [Общие итоги миграции населения УР за 2012 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/58cd2b004f38203db681fe3a99b5ae2d/%D0%9E%D0%B1%D1%89%D0%B8%D0%B5+%D0%B8%D1%82%D0%BE%D0%B3%D0%B8+%D0%BC%D0%B8%D0%B3%D1%80%D0%B0%D1%86%D0%B8%D0%B8+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%B7%D0%B0+%D1%8F%D0%BD%D0%B2%D0%B0%D1%80%D1%8C-%D0%B4%D0%B5%D0%BA%D0%B0%D0%B1%D1%80%D1%8C+2012.zip);
- [Общие итоги миграции населения УР за 2013 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/ccbdc78043926504945994d06954faf7/%D0%9E%D0%B1%D1%89%D0%B8%D0%B5+%D0%B8%D1%82%D0%BE%D0%B3%D0%B8+%D0%BC%D0%B8%D0%B3%D1%80%D0%B0%D1%86%D0%B8%D0%B8+%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F+%D0%B7%D0%B0+%D1%8F%D0%BD%D0%B2%D0%B0%D1%80%D1%8C-%D0%B4%D0%B5%D0%BA%D0%B0%D0%B1%D1%80%D1%8C+2013.zip);
- [Общие итоги миграции населения УР за 2014 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/9776af0047bf6038b227b6ed3bc4492f/%D0%9C%D0%B8%D0%B3%D1%80_2014.pdf);
- [Общие итоги миграции населения УР за 2015 г.](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/e33cf9804c66630c927b937dff7d05ed/%D0%9C%D0%B8%D0%B3%D1%80_2015.pdf);
- [Основные демографические показатели УР в 1970-2015](http://udmstat.gks.ru/wps/wcm/connect/rosstat_ts/udmstat/resources/3173df804c0f7c8a96c7f7b4bce00d93/%D0%9E%D1%81%D0%BD-%D0%B4%D0%B5%D0%BC-%D0%BF%D0%BE%D0%BA%D0%A3%D0%A0_1970-2015.pdf).
