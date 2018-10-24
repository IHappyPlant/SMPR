


# Теория машинного обучения

## Навигация
1. [Метрические алгоритмы классификации](#метрические-алгоритмы-классификации)
	1.1 [Метод *k* ближайших соседей](#метод-k-ближайших-соседей-knn)
	1.2 [Метод  *k*  взвешенных ближайших соседей (kwNN)](#метод-k-взвешенных-ближайших-соседей-kwnn)
	1.3 [Метод парзеновского окна](#метод-парзеновского-окна)

## Метрические алгоритмы классификации
Метрические алгоритмы классификации - алгоритмы, основанные на вычислении оценок сходства между объектами. Одним из вариантов сходства является расстояние между объектами. Тогда вводится функция расстояния ![](http://latex.codecogs.com/gif.latex?%24%5Crho%20%28x_1%2C%20x_2%29%24).
### Метод *k* ближайших соседей (*kNN*)
Один из самых простых метрических алгоритмов классификации. Работает следующим образом: пусть дан классифицируемый объект *z* и обучающая выборка ![](http://latex.codecogs.com/gif.latex?%24X%5El%24). Требуется определить класс объекта *z* на основе данных из обучающей выборки. Для этого:
1. Вся выборка ![](http://latex.codecogs.com/gif.latex?%24X%5El%24) сортируется по возрастанию расстояния от объекта *z* до каждого объекта выборки.
2. Проверяются классы *k* ближайших соседей объекта *z*. Класс, встречаемый наиболее часто, присваивается объекту *z*.  

Вход алгоритма - классифицируемый объект, обучающая выборка и параметр *k* - число рассматриваемых ближайших соседей.
Выход алгоритма - класс классифицируемого объекта.

При *k = 1* алгоритм превращается в *1NN*, то есть объекту *z* присваивается класс его первого ближайшего соседа, все остальные объекты выборки не учитываются.

Оптимальное значение *k* подбирается по Критерию Скользящего Контроля (*LOO*). Суть критерия: пусть дана обучающая выборка ![](http://latex.codecogs.com/gif.latex?%24X%5El%24). Требуется определить оптимальное значение параметра *k* для данной выборки. Для этого:
1. Из выборки удаляется *i*-й объект ![](http://latex.codecogs.com/gif.latex?%24x%5Ei%24).
2. Выбранный алгоритм классификации запускается для ![](http://latex.codecogs.com/gif.latex?%24x%5Ei%24) и оставшейся выборки. По окончании работы алгоритма полученный класс объекта ![](http://latex.codecogs.com/gif.latex?%24x%5Ei%24) сравнивается с его реальным классом. При их несовпадении сумма ошибки увеличивается на 1.
3. Шаги 1 и 2 повторяются для каждого объекта выборки при фиксированном *k*. По окончании работы алгоритма полученная сумма ошибки *sum* делится на размер выборки *l*: ![sum=sum/l](http://latex.codecogs.com/gif.latex?sum%3D%20%5Cfrac%7Bsum%7D%7Bl%7D) .  Потом значение *k* меняется, и алгоритм повторяется для нового значения. *k* с наименьшим значением суммы ошибки будет оптимальным.
#### Реализация
При реализации алгоритма, в качестве обучающей выборки использовалась выборка ирисов Фишера. В качестве признаков объектов использовались значения длины и ширины лепестка. Значение *k* подбиралось по *LOO*.

Алгоритм:

    kNN <- function(xl, k, z) {
	  orderedXL <- sortObj(xl, z)
	  n <- dim(orderedXL)[2]
	  classes <- orderedXL[1:k, n] 
	  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
	  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
	  return (class)
	}
где *xl* - обучающая выборка.

![kNN.png](https://github.com/IHappyPlant/RProjects/blob/master/img/kNN_plot.png)

Достоинства алгоритма:
1. Простота реализации
2. Хорошее качество, при правильно подобранной метрике и параметре *k*

Недостатки алгоритма:
1. Необходимость хранить выборку целиком
2. Малый набор параметров
3. Качество классификации сильно зависит от выбранной метрики

### Метод *k* взвешенных ближайших соседей (*kwNN*)
Отличается от *kNN* тем, что вес ближайших соседей зависит не от ранга соседа, а от расстояния до объекта *z*. В *kNN* каждый из *k* ближайших соседей имеет вес равный единице, а все остальные объекты выборки имеют вес, равный нулю. Поэтому можно было говорить о *частоте* появления класса среди *k* ближайших соседей. В методе *kwNN* для задания весов *k* ближайшим соседям должна использоваться невозрастающая функция. Мы будем использовать функцию весов ![w(i)=q^i](http://latex.codecogs.com/gif.latex?w%28i%29%3Dq%5Ei) , *i* - ранг соседа, *q* - подбираемый параметр. Значение *q* подбирается для каждого *k* по *LOO*.
#### Реализация
При реализации использовалась та же выборка ирисов Фишера. Значения *k* и *q* подбирались по *LOO*  
Алгоритм:

    kwNN <- function(xl, z, k, q) {
	  orderedXL <- sortObj(xl, z);
	  n <- ncol(orderedXl)
	  classes <- orderedXL[1:k, n]     # Берём k ближайших соседей
	  classes <- table(classes)        # Делаем для них таблицу
	  classes[1:length(classes)] <- 0  # Обнуляем все значения в таблице
	  for (i in names(classes)) {      # Для каждого класса
	    for (j in 1:k) {               # Проходим по всем k соседям
	      if (orderedXL[j, n] == i)    # И суммируем веса всех объектов одинаковых классов
	        classes[i] = classes[i] + q^j
	    }
	  }
	  class <- names(which.max(classes)) # Вернём самый большой вес
	  return (class)
	}
где *xl* - обучающая выборка.

![kwNN.png](https://github.com/IHappyPlant/RProjects/blob/master/img/kwNN_plot.png) 


Достоинства алгоритма
1. Простота реализации
2. Хорошая эффективность при правильно подобранной метрике, *k* и *q*. В общем случае, более высокая эффективность, чем у *kNN*.

Недостатки. Вообще говоря, те же, что и у *kNN*:
1. Необходимость хранить выборку целиком
2. Малый набор параметров
3. Сильная зависимость качества классификации от выбранной метрики (особенность всех метрических алгоритмов)

На выборке ирисов Фишера kwNN показывает такую же эффективность, как и kNN. Поэтому, чтобы показать более высокую эффективность kwNN, подберём выборку вруную так, чтобы kNN ошибся при классификации, а kwNN - нет.
Приведём пример:

![kNN_vs_kwNN.png](https://github.com/IHappyPlant/RProjects/blob/master/img/kNN_vs_kwNN.png) 

При k = 5 точку с координатами (5; 5) kNN отнёс с синему классу, так как синих объектов среди k ближайших соседей больше, а kwNN при k = 5, q = 0.1, отнёс её к классу красных объектов, так как они расположены ближе к точке, и влияют на классификацию сильнее.

### Метод парзеновского окна
Идея этого алгоритма в том, чтобы присваивать вес каждому объекту выборки не на основе ранга близости *i-го* объекта к классифицируемому, а на основе *расстояния* от классифицируемого объекта до данного объекта выборки. В отличии от метода ближайших соседей, сортировка объектов выборки по расстоянию здесь не нужна.
Функция веса выглядит следующим образом: ![](http://latex.codecogs.com/gif.latex?w%28i%2C%20z%29%20%3D%20K%28%5Cfrac%7B%5Crho%28z%2C%20x_%7Bi%7D%29%7D%7Bh%7D%29)
Где *i* - номер объекта выборки, *z* - классифицируемый объект, *xi* - *i-й* объект выборки, *h* - ширина окна, *K* - функция ядра.

Функция ядра - произвольная чётная функция, невозрастающая на *[0, +inf]*. На практике применяются следующие функции ядра:
1.  ![](http://latex.codecogs.com/gif.latex?K%28r%29%20%3D%20P%28r%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%5B%7Cr%7C%20%3C%3D%201%5D) - Прямоугольное ядро
2.  ![](http://latex.codecogs.com/gif.latex?K%28r%29%20%3D%20T%28r%29%20%3D%20%281%20-%20%7Cr%7C%29%5B%7Cr%7C%3C%3D1%5D) - Треугольное ядро
3.  ![](http://latex.codecogs.com/gif.latex?K%28r%29%20%3D%20E%28r%29%20%3D%20%5Cfrac%7B3%7D%7B4%7D%281%20-%20r%5E%7B2%7D%29%5B%7Cr%7C%3C%3D1%5D) - Ядро Епанечникова
4.  ![](http://latex.codecogs.com/gif.latex?K%28r%29%20%3D%20Q%28r%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D%281%20-%20r%5E%7B2%7D%29%5E%7B2%7D%5B%7Cr%7C%3C%3D1%5D) - Квартическое ядро
5.  ![](http://latex.codecogs.com/gif.latex?K%28r%29%20%3D%20G%28r%29%20%3D%20%282%5Cpi%29%5E%7B-%5Cfrac%7B1%7D%7B2%7D%7De%5E%7B%28-%5Cfrac%7B1%7D%7B2%7Dr%5E%7B2%7D%29%7D) - Гауссовское ядро

Где ![](http://latex.codecogs.com/gif.latex?r%3D%5Cfrac%7B%5Crho%28z%2C%20x_%7Bi%7D%29%7D%7Bh%7D)

#### Программная реализация алгоритма:

    parzen <- function(xl, h, distances, kernelFunction = kernel.G) {
      # xl - выборка
      # h - ширина окна
      # distances - расстояния от объекта z до каждого объекта из xl 
      # Расстояния считаются заранее, поэтому сам объект z здесь не нужен
      # kernelFunction - используемая функция ядра. По умолчанию Гауссовское
      l <- nrow(xl)
      n <- ncol(xl)
      classes <- xl[1:l, n] # Классы объектов выборки
      weights <- table(classes) # Таблица для весов классов
      weights[1:length(weights)] <- 0
      for (i in 1:l) { # Для каждого объекта выборки
        class <- xl[i, n] # Берём его класс
        r <- distances[i] / h
        weights[class] <- weights[class] + kernelFunction(r) # И прибавляем его вес к общему весу его класса
      }
      if (max(weights) == 0) # Если точка не попала в окно (не проклассифицировалась)
        return (0) # То вернуть 0
      class <- names(which.max(weights))
      return (class) # Иначе вернуть класс с максимальным весом
    }
Значение *h* подбиралось в пределах от *0.1* до *2* по *LOO*. Алгоритм тестировался на выборке ирисов Фишера для разных функций ядра:

*Прямоугольное ядро:*
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_P.png)
*Треугольное ядро:*
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_T.png)
*Ядро Епанечникова:*
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_E.png)
*Квартическое ядро:*
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_Q.png)
*Гауссовское ядро:*
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_G.png)

У всех ядер, кроме Гауссовского один и тот же недостаток: они не способны проклассифицировать точки, не попавшие в окно (те, для которых *|r| > 1*). Значение *h* имеет смысл подбирать в пределах небольших значений, т. к. при большом значении *h* слишком много точек попадает в окно, и качество классификации падает *крайне* значительно. Для примера приведём карту классификации для Гауссовского ядра при попытке выбора *h* по *LOO* в пределах от *1* до *l (длина выборки)*:
![](https://github.com/IHappyPlant/RProjects/blob/master/img/parzen_plot_badH.png)

Достоинства алгоритма:
1. Простота реализации
2. Высокое качество классификации при правильно подобранном *h*
3. Для классификации объекта не требуется сортировка выборки, что сокращает время классификации

Недостатки алгоритма:
1. Необходимость хранить выборку целиком
2. Малый набор параметров
3. Если ни один объект выборки не попал в радиус окна *h* вокруг объекта *z*, то алгоритм не способен его проклассифицировать. Исправляется использованием Гауссовского ядра.

