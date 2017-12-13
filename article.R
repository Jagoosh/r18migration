
# setup_project -----------------------------------------------------------

library(tidyverse)
library(ggrepel)

theme_set(theme_bw(base_size = 10))

# read_data ---------------------------------------------------------------

DB_population <- read_csv("data/DB_population.csv")
DB_events <- read_csv("data/DB_events.csv")

inf_area <- data_frame(
  levels = c(
    "Всего по республике", "Городские поселения", "Городские округа", 
    "Городские округа без г. Ижевска", "г. Ижевск", "г. Ижевск и Завьяловский р-н", 
    "г. Воткинск", "г. Глазов", "г. Можга", "г. Сарапул", 
    "Сельская местность", "Муниципальные районы", 
    "Муниципальные районы без Завьяловского", 
    "Алнашский", "Балезинский", "Вавожский", 
    "Воткинский", "Глазовский", "Граховский", "Дебесский", 
    "Завьяловский", "Игринский", "Камбарский", 
    "Каракулинский", "Кезский", "Кизнерский", 
    "Киясовский", "Красногорский", "Малопургинский", 
    "Можгинский", "Сарапульский", "Селтинский", 
    "Сюмсинский", "Увинский", "Шарканский", 
    "Юкаменский", "Якшур-Бодьинский", "Ярский"
  ),
  labels = c(
    "Удмуртская Республика", "Городские поселения", "Городские округа", 
    "Городские округа без г.Ижевска", "г.Ижевск", "г.Ижевск и Завьяловский р-н", 
    "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул", 
    "Сельская местность", "Муниципальные районы", 
    "Муниципальные районы без Завьяловского", 
    "Алнашский", "Балезинский", "Вавожский", 
    "Воткинский", "Глазовский", "Граховский", "Дебесский", 
    "Завьяловский", "Игринский", "Камбарский", 
    "Каракулинский", "Кезский", "Кизнерский", 
    "Киясовский", "Красногорский", "Малопургинский", 
    "Можгинский", "Сарапульский", "Селтинский", 
    "Сюмсинский", "Увинский", "Шарканский", 
    "Юкаменский", "Якшур-Бодьинский", "Ярский"
  )
)

inf_gender <- data_frame(
  levels = c("male", "female", "both"),
  labels = c("мужчины", "женщины", "всего")
)

# preparation_data --------------------------------------------------------

# - добавлены суммарные данные муниципальных районов;

DB_population <- bind_rows(
  DB_population %>% 
    filter(area %in% c(
      "Алнашский", "Балезинский", "Вавожский", "Воткинский", "Глазовский",
      "Граховский", "Дебесский", "Завьяловский", "Игринский", "Камбарский",
      "Каракулинский", "Кезский", "Кизнерский", "Киясовский", "Красногорский",
      "Малопургинский", "Можгинский", "Сарапульский", "Селтинский",
      "Сюмсинский", "Увинский", "Шарканский", "Юкаменский",
      "Якшур-Бодьинский", "Ярский"
    )) %>% 
    group_by(n_year, age, gender) %>% 
    summarise(area = "Муниципальные районы", value = sum(value)) %>% 
    ungroup(),
  DB_population
)

# - добавлены суммарные данные городских округов;

DB_population <- bind_rows(
  DB_population %>% 
    filter(area %in% c(
      "г. Ижевск", "г. Воткинск", "г. Глазов", "г. Можга", "г. Сарапул"
    )) %>% 
    group_by(n_year, age, gender) %>% 
    summarise(area = "Городские округа", value = sum(value)) %>% 
    ungroup(),
  DB_population
)

# - добавлены суммарные данные городских округов без г. Ижевска;

DB_population <- bind_rows(
  DB_population %>% 
    filter(area %in% c(
      "г. Воткинск", "г. Глазов", "г. Можга", "г. Сарапул"
    )) %>% 
    group_by(n_year, age, gender) %>% 
    summarise(area = "Городские округа без г. Ижевска", value = sum(value)) %>% 
    ungroup(),
  DB_population
)

# - добавлены данные агломерации г. Ижевска и Завьяловского муниципального района;

DB_population <- bind_rows(
  DB_population %>% 
    filter(area %in% c(
      "г. Ижевск", "Завьяловский"
    )) %>% 
    group_by(n_year, age, gender) %>% 
    summarise(area = "г. Ижевск и Завьяловский р-н", value = sum(value)) %>% 
    ungroup(),
  DB_population
)

# - добавлены данные муниципальных районов без Завьяловского;

DB_population <- bind_rows(
  DB_population %>% 
    filter(area %in% c(
      "Алнашский", "Балезинский", "Вавожский", "Воткинский", "Глазовский",
      "Граховский", "Дебесский", "Игринский", "Камбарский",
      "Каракулинский", "Кезский", "Кизнерский", "Киясовский", "Красногорский",
      "Малопургинский", "Можгинский", "Сарапульский", "Селтинский",
      "Сюмсинский", "Увинский", "Шарканский", "Юкаменский",
      "Якшур-Бодьинский", "Ярский"
    )) %>% 
    group_by(n_year, age, gender) %>% 
    summarise(area = "Муниципальные районы без Завьяловского", value = sum(value)) %>% 
    ungroup(),
  DB_population
)

# - добавлены суммарные данные обоих полов;

DB_population <- 
  DB_population %>% 
  spread(gender, value) %>% 
  mutate(both = male + female) %>% 
  gather(gender, value, both, male, female)

DB_population <- DB_population %>% 
  mutate(
    area = factor(
      area,
      levels = inf_area$levels,
      labels = inf_area$labels
    ),
    gender = factor(
      gender,
      levels = inf_gender$levels,
      labels = inf_gender$labels
    )
  )

DB_events <- 
  DB_events %>% 
  mutate(
    area = factor(
      area,
      levels = inf_area$levels,
      labels = inf_area$labels
    ),
    gender = factor(
      gender,
      levels = inf_gender$levels,
      labels = inf_gender$labels
    )
  )

AA_population <- 
  inner_join(
    DB_population,
    DB_population %>% 
      mutate(n_year = n_year - 1, age = age - 1),
    by = c("area", "age", "gender", "n_year")
  ) %>% 
  mutate(value = (value.x + value.y) / 2) %>% 
  select(- value.x, - value.y) %>% 
  rename(year = n_year)

# illustration_age --------------------------------------------------------

population_events <- DB_events %>% 
  filter(event != "born", !is.na(area)) %>% 
  spread(event, value) %>% 
  mutate(net = arrive - depart, died = - died, depart = - depart) %>% 
  gather(event, value, - year, - age, - area, - gender, na.rm = TRUE) %>% 
  inner_join(
    AA_population,
    by = c("age", "area", "gender", "year")
  ) %>% 
  mutate(value = 1000 * value.x / value.y) %>% 
  group_by(age, area, gender, event) %>% 
  summarise(mean = mean(value), lwr = min(value), upr = max(value))

population_events %>% 
  filter(area == "Удмуртская Республика", age <= 70, gender != "всего") %>% 
  ggplot(aes(x = age)) +
  geom_line(aes(y = mean, color = gender, linetype = "mean")) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = gender), alpha = 0.3) +
  geom_vline(xintercept = c(15, 18, 30), linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = c(0, 15, 18, 30, 70), minor_breaks = 1:70) +
  facet_grid(event ~ area, labeller = as_labeller(c(
    'Удмуртская Республика' = "Удмуртская Республика",
    'died' = "смертность",
    'arrive' = "прибытие",
    'depart' = "выбытие",
    'net' = "нетто-\nмиграция"
  ))) +
  labs(
    title = "Движение населения",
    x = "Возраст, лет",
    y = "Изменение, ‰"
  ) +
  scale_linetype_manual(
    name = "Значение",
    values = "solid",
    breaks = "mean",
    labels = "среднее\nарифметическое"
  ) +
  scale_color_grey(name = "Пол") +
  scale_fill_grey(name = "Пол")

# illustration_cohort -----------------------------------------------------

DB_population %>% 
  filter(
    age >= 10, age <= 30,
    gender %in% c("мужчины", "женщины"),
    area == "г.Глазов"
  ) %>% 
  mutate(
    # age_2016 = as.character(2016 - n_year + age + 1)
    b_year = as.character(n_year - age)
  ) %>% 
  filter(as.integer(b_year) >= 1991, as.integer(b_year) <= 1995) %>% 
  ggplot(aes(x = age, y = value, color = b_year)) +
  geom_line() +
  scale_x_continuous(minor_breaks = 0:60) +
  scale_color_grey() +
  facet_grid(gender ~ area) +
  labs(
    title = "Динамика численности населения одного пола и года рождения\nв 2012—2016 гг.",
    x = "Возраст, лет",
    y = "Численность, человек",
    color = "Год рождения"
  )

# selection_data ----------------------------------------------------------

# - исключены из обработки:
#   + сведения об отдельных частях муниципальных образований;
#   + сведения о возрастах, превышающих 80 лет;

DB_population <- DB_population %>% 
  filter(
    ! area %in% c(
      "г. Камбарка", "Индустриальный", "Ленинский",
      "Октябрьский", "Первомайский", "Устиновский"
    ),
    ! is.na(area),
    age <= 80
  )

# - реальные поколения (люди одного года рождения)
# ассоциированы с переменной, хранящей их возраст в 2016 году;
# - тип сведений о возрасте на год измерения и возрасте в 2016 году
# изменен на категориальный;

DB_population <- DB_population %>% 
  mutate(
    age_2016 = as.character(2016 - n_year + age + 1),
    age = as.character(age)
  )

# - данные сгруппированы по месту жительства и полу.

population_nested <- DB_population %>% 
  group_by(area, gender) %>%
  nest()

# fit_models --------------------------------------------------------------

population_function <- function(df) {
  lm(value ~ age + age_2016, data = df)
}

population_models <- population_nested %>%
  mutate(
    model = data %>% map(population_function),
    glance = model %>% map(broom::glance),
    tidy = model %>% map(broom::tidy, conf.int = TRUE, conf.level = 0.95),
    augment = model %>% map(broom::augment)
  )

# evaluation_model --------------------------------------------------------

population_fq <- population_models %>% 
  unnest(glance) %>% 
  select(area, gender, adj.r.squared, statistic, p.value, AIC) %>% 
  arrange(area)

population_fq %>% 
  mutate(label = ifelse(
    area %in% c(
      "Удмуртская Республика", "Городские поселения", 
      "Городские округа без г.Ижевска", "г.Ижевск", "г.Ижевск и Завьяловский р-н", 
      "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул", 
      "Сельская местность", "Муниципальные районы", 
      "Муниципальные районы без Завьяловского"
    ),
    as.character(area),
    NA
  )) %>% 
  ggplot(aes(x = adj.r.squared, y = AIC, label = label)) +
  geom_point(aes(color = gender)) +
  geom_text_repel(
    segment.color = "black", size = 2.5, max.iter = 200000,
    min.segment.length = unit(0.01, "lines"), na.rm = TRUE
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_grey(name = "Пол") +
  facet_wrap(~ gender, dir = "h") +
  guides(color = "none") +
  labs(
    title = "Оценка качества моделей",
    x = "Скорректированный R²",
    y = "AIC"
  )

# filtration_area ---------------------------------------------------------

DB_population %>% 
  filter(
    area %in% c(
      "Муниципальные районы", "Сельская местность",
      "Городские округа", "Городские поселения"
    )
  ) %>% 
  group_by(n_year, gender, area) %>% 
  summarise(value = sum(value)) %>% 
  spread(area, value) %>% 
  transmute(
    rurar = `Муниципальные районы` / `Сельская местность`,
    urban = `Городские округа` / `Городские поселения`
  ) %>% 
  gather(area, value, urban, rurar) %>% 
  ungroup() %>% 
  mutate(
    date = as.Date(paste0(n_year, "-01-01")),
    area = factor(
      area,
      levels = c("urban", "rurar"),
      labels = c("Городские территории", "Сельские территории")
    )
  ) %>% 
  ggplot(aes(x = date, y = value, color = gender)) +
  geom_hline(yintercept = c(1), linetype = 2, alpha = 0.2) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%d.%m.%Y", minor_breaks = NULL) +
  scale_color_grey(name = "Пол") +
  facet_grid(area ~ gender) +
  guides(color = "none") +
  labs(
    title = "Стабильность состава групп територий",
    x = "Дата",
    y = "Отношение численности населения\nгрупп территорий"
  )

# illustration_newborn ----------------------------------------------------

newborn_model <- population_models %>%
  filter(gender == "всего", area == "Удмуртская Республика") %>%
  .$model

newborn_data <- DB_events %>% 
  filter(gender == "всего", area == "Удмуртская Республика", age == 0) %>% 
  spread(event, value) %>% 
  mutate(
    live = born - died,
    age_2016 = as.character(2016 - year),
    age = as.character(age)
  ) %>% 
  select(- arrive, - depart, -born)

newborn_fitted <- cbind(
  newborn_data,
  predict(
    newborn_model[[1]],
    newborn_data[, c("age", "age_2016")],
    interval = "prediction", level = 0.95
  )
) %>% 
  gather(variable, value, live, fit)

ggplot(newborn_fitted, aes(x = year)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = gender), alpha = .2) +
  geom_line(aes(y = value, linetype = variable, color = gender)) +
  scale_x_continuous(minor_breaks = 1990:2015) +
  scale_color_grey(
    name = "Пол"
    # breaks = c("both", "male", "female"),
    # labels=c("оба", "муж", "жен")
  ) +
  scale_fill_grey(
    name = "Пол"
    # breaks = c("both", "male", "female"),
    # labels=c("оба", "муж", "жен")
  ) +
  scale_linetype_manual(
    name = "Значение",
    values = c("dashed", "solid"),
    breaks = c("live", "fit"),
    labels=c("фактическое,\nбез умерших младше года", "по оценке модели")
  ) +
  facet_wrap( ~ area) +
  labs(
    title = "Новорожденные Удмуртской Республики",
    x = "Год рождения",
    y = "Численность, чел."
  )

# age_analize -------------------------------------------------------------

population_estimate <- 
  population_models %>%
  unnest(tidy) %>%
  select(area, gender, term, conf.low, conf.high, estimate)

population_age <- 
  population_estimate %>%
  filter(grepl("age[0-9]", term)) %>%
  mutate(
    age = str_replace(term, "age", "") %>% as.numeric()
  )

plot_age <- function(area4plot) {
  population_age %>%
    filter(
      age >= 0, age <= 30,
      area %in% area4plot,
      gender %in% c("мужчины", "женщины")
    ) %>%
    ggplot(aes(x = age)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = gender), alpha = 1/3) +
    geom_line(aes(y = estimate, color = gender)) +
    geom_vline(xintercept = c(15, 18, 30), linetype = 2, alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.2) +
    scale_x_continuous(breaks = c(0, 15, 18, 30, 40), minor_breaks = 0:40) +
    facet_wrap(~ area, ncol = 2) +
    labs(
      title = "Влияние возраста на численность возрастно-половой группы",
      x = "Возраст как фактор, лет",
      y = "Эффект фактора, чел."
    ) +
    scale_color_grey(
      name = "Пол"
    ) +
    scale_fill_grey(
      name = "Пол"
    )
}

plot_age(area = c(
  "Удмуртская Республика", "г.Ижевск", "Городские округа без г.Ижевска", "Муниципальные районы"
))

plot_age(area = c(
  "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул"
))

# mean_population ---------------------------------------------------------

DB_population %>% 
  filter(
    area %in% c(
      "Муниципальные районы", "Сельская местность",
      "Городские округа", "Городские поселения"
    ),
    gender == "всего"
  ) %>% 
  group_by(n_year, area) %>% 
  summarise(value = sum(value)) %>% 
  spread(n_year, value)

# newborn_2016 ------------------------------------------------------------

population_estimate %>% 
  filter(
    str_detect(term, "Intercept"),
    area %in% c(
      "Удмуртская Республика", "г.Ижевск", "Городские округа без г.Ижевска",
      "Муниципальные районы", "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул"
    ),
    gender != "всего"
  ) %>% 
  select(area, gender, estimate) %>% 
  spread(gender, estimate)


# died_before_30 ----------------------------------------------------------

DB_events %>% 
  filter(
    event == "died",
    age <= 30,
    area %in% c(
      "Удмуртская Республика", "г.Ижевск", "Городские округа без г.Ижевска",
      "Муниципальные районы", "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул"
    ),
    gender != "всего"
  ) %>% 
  group_by(area, gender, age) %>% 
  summarise(value = mean(value)) %>% 
  group_by(area, gender) %>% 
  summarise(value = sum(value)) %>% 
  spread(gender, value)

# approximation_newborn ---------------------------------------------------

new_data <- 
  data_frame(
    age = "0",
    age_2016 = as.character(2016 - 1986:2015)
  )

approximation_newborn <- 
  population_models %>% 
  mutate(
    fit = model %>% 
      map(function(xx) {
        
        bind_cols(
          new_data %>% 
            mutate_all(as.numeric),
          predict(
            xx,
            newdata = new_data,
            interval = "prediction", level = 0.95
          ) %>% 
            as_data_frame()
        )
      }
      )
  ) %>% 
  select(-data, -model, -glance, -tidy, -augment) %>% 
  unnest(fit)

plot_newborn <- function(area4plot) {
  approximation_newborn %>% 
    filter(
      gender == "всего",
      area %in% area4plot
    ) %>% 
    mutate(year = 2016 - age_2016 + age) %>% 
    ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = gender), alpha = .2) +
    geom_line(aes(y = fit, color = gender)) +
    scale_x_continuous(minor_breaks = 1986:2015) +
    scale_color_grey(
      name = "Пол"
      # breaks = c("both", "male", "female"),
      # labels=c("оба", "муж", "жен")
    ) +
    scale_fill_grey(
      name = "Пол"
      # breaks = c("both", "male", "female"),
      # labels=c("оба", "муж", "жен")
    ) +
    facet_wrap( ~ area) +
    labs(
      title = "Новорожденные",
      x = "Год рождения",
      y = "Численность, чел."
    )
}

plot_newborn(area = c(
  "Удмуртская Республика", "г.Ижевск", "Городские округа без г.Ижевска", "Муниципальные районы"
))

plot_newborn(area = c(
  "г.Воткинск", "г.Глазов", "г.Можга", "г.Сарапул"
))
