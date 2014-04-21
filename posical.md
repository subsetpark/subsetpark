title: Posical, the Positivist Calendar for Python

Let's begin 225 years ago. In the year 1789 of the conventional [Gregorian calendar](http://en.wikipedia.org/wiki/Gregorian_calendar), widespread crisis and dissent forced the Estates-General to convene in France for the first time in 175 years. Thus began the French Revolution.

60 years after that, in that same nation, the philosopher [August Comte](http://en.wikipedia.org/wiki/Auguste_Comte) proposed a [calendar reform](http://en.wikipedia.org/wiki/Calendar_reform) known as the [Positivist Calendar](http://en.wikipedia.org/wiki/Positivist_Calendar).

## Here Are Some of the Reasons That the Gregorian Calendar is Awful

The kludginess of the commonly used Gregorian Calendar will be immediately apparent to anybody who has had to use it, which population happens to include the majority of persons living in the world today. 

The inopportuneness of the scheme lies primarily in its division of months. There's the leap year thing to contend with, of course; but that's unavoidable as long as one prefers one's days 24 hours long. The months, on the other hand, are more or less unforgivable:

- They're of irregular length. Not only are some 30 days long and some 31 days long, which seems (I say seems) to be unavoidable if you've decided you like a thirtyish-day-long month and you've got 365 days to give out, but one is only 28 days long. So in fact their length is more varied than it would need to be under nearly any constraint.

- They're of irregular distribution. As aficionados of the [knuckle technique](http://www.wikihow.com/Memorize-How-Many-Days-Are-in-Each-Month) know, the months of the year roughly *tend* towards alternating, 30 and 31 days, but there happens to be a missing 30-day month between July and August.

Secondarily we have to take issue with the distribution of weeks. 

- They neither fit neatly into the months, leaving two or three left over for each month of the year (except, perversely, that outlier, February, which contains exactly four weeks), nor do they sit easily with the year, leaving us with 52 weeks and one lonely day for every year.

These facts would seem to merit immediate and absolute rectification to nearly anybody given how inconvenient they make the very basic and very essential practice of telling time, but in fact they tend to be taken for granted by nearly everybody.

## There is a Better Way, or Several

Nearly everybody, of course, but August Comte, and his ilk. Msr. Comte was neither the first nor the last person to propose a more rational alternative to the calendar, but he is the one we'll be paying attention to today. 

### Basic Principles for Calendar Design

No matter who you are, if you decide you could do better than 16th century astronomer [Aloysius Lilius of Calabria](http://en.wikipedia.org/wiki/Aloysius_Lilius) and [Pope Gregory XIII](http://en.wikipedia.org/wiki/Pope_Gregory_XIII), there are some basic mathematical truths you're contending with. Chief among them is this: 

#### There are 365 days in a year (366 on leap years)

Neither Gregory nor Comte nor anybody else decided on the ratio of the rotational period of the Earth to its orbital period. That one we're stuck with. This is not a number that lends itself to factorization. It has exactly two factors aside from 1 and itself: 5 and 73. This makes division difficult.

Here is a secondary truth:

#### There are 7 days in a week

This is, of course, up for debate; in fact in a way it's significantly more trivial than the months themselves. If we decided we wanted five days in a week, say, then we'd have exactly 73 weeks in the year. Then again, 73 is prime, so our neat subdivision would have to end there.

In any case the fact of a 7-day week has remained more or less unaffected by the procession of calendars in the West and it doesn't really seem up for debate. Whether it's Thursday tends to have much more of an impact on most people than whether it's March or April. Or Archimedes, as we'll see.

There is however another truth that we can bear in mind as we fix the mistakes of the past:

#### 364 is really, really close to 365 - they're really almost the same thing

Bear with me. 

Here is the neat calendrical trick, first come up with by [Hugh Jones](http://en.wikipedia.org/wiki/Hugh_Jones_(reverend)), that suddenly allows the calendar to breathe more easily in all directions. 365 mod 7 = 1, that is, there's one day left after the 52 whole weeks in a year. 364 is perfectly divisible by 7. But unlike 365, which has only a week-sized factor, 364 has a week-sized factor and a month-sized factor: 28. And, in a stroke of luck so good that one must imagine that Rev. Jones was more divinely favored than His 16th C. Holiness himself, 28 is *also* perfectly divisible by 7.

What this means is that if we had only 364 days to deal with, then we could divide them into 13 equally-sized months, each of which contained 4 equally-sized weeks. Then the first of every month would be Monday, the 18th would always be Thursday, the 180th day of the year would always always be Saturday, et cetera. 

Then, let us only deal with 364 days. In the Positivist Calendar, the 365th day (and 366th day on Leap Years) is not a part of any month at all, nor a day of the week. It is [*epagomenal*](http://en.wikipedia.org/wiki/Intercalation_(timekeeping)). The 365th day of a year is just the 365th of that particular year; a day set aside from the ordinary cycle before things begin again.

## A Calendar for a More Rational Age

So there you have it. The Positivist Calendar consists of 13 28-day months, with 1 epagomenal (or intercalary) day, two in a leap year. This means that every month is the same length, and the weekdays of every month are always the same. Finally it is *perennial*, in that the weekdays of every year are always the same.

Comte's Positivist Calendar has a few extra features for added delight and interest. In a further break from the Gregorian format, he renamed all the months and days of his calendar after his own humanist pantheon. So the eighth month of the Positivist Calendar is called Dante, the sixth is Moses (not a completely irreligious choice, I'll allow), and so on. And the 10th of Dante is dedicated to Rembrandt, the 8th of Shakespeare is dedicated to [Tirso](http://en.wikipedia.org/wiki/Tirso_de_Molina), and on down the line. The epagomenal day is known simply as *The Festival of All the Dead*. 

Finally: Comte set year 1 of his new calendar as the first year of the truly modern age: 1789, the first year of the Great Crisis.

Thus it is that I write to you on this day, Wednesday, the 24th of Archimedes, in the Positivist year of 225, dedicated in honor of [Vitruvius](http://en.wikipedia.org/wiki/Vitruvius). 

# Posical

I've written a Python library, [posical](https://github.com/subsetpark/posical), to model and explore the Positivist calendar. My initial aim was to create `AlternateDate` objects that could interact with [`date` objects](https://docs.python.org/3.3/library/datetime.html#datetime.date) produced by the standard `datetime` library. 

```python
class AlternateDate(object):

    def __init__(self, year, month, day, calendar):
        self.year = year
        self.month = month
        self.day = day
        self.day_of_year = (month - 1) * calendar.weeks_in_a_month * calendar.days_in_a_week + day
        if self.day_of_year > 366:
            raise ValueError("This day cannot exist.")

        self.is_leap = calendar.is_leap(self.to_gregorian().year)
                        
        self.weekday = calendar.get_weekday(self.day)
        self.month_name = calendar.get_month_name(self.month)
        self.day_name = calendar.get_day_name(self.day_of_year, self.is_leap)
        self.weekday_name = calendar.get_weekday_name(self.weekday)
```

## Date Interactions

The AlternateDate class has methods that take care of two types of interaction with standard `datetime` classes: date comparison and timedelta math. Standard date objects are able to interact with each other, and do things like the following:

```python
>>> import datetime
>>> d = datetime.date(2014, 4, 18)
>>> d1 = datetime.date(2014, 4, 19)
>>> d1 > d
True
>>> d1 - d
datetime.timedelta(1)
>>> 
```

That is, you can compare two dates to see which is 'greater' (in the sense of later, or I suppose, containing more days since the start of time), and you can also work with timedeltas. The `timedelta` class represents the absolute difference between two times, expressed (when it comes to date math) as a number of days. So tomorrow minus today results in a timedelta of 1 day. And tomorrow plus a timedelta of 1 day equals a date object representing the day after tomorrow.

```python
def __add__(self, arg):
    return self.calendar.from_date(arg + self.to_gregorian())
__radd__ = __add__
def __sub__(self, arg):
    return self.calendar.from_date(self.to_gregorian() - arg)
def __rsub__(self, arg):
    return self.calendar.from_date(arg - self.to_gregorian())
def __eq__(self, other_date):
    return other_date == self.to_gregorian()
def __gt__(self, other_date):
    return self.to_gregorian() > other_date
def __lt__(self, other_date):
    return self.to_gregorian() < other_date
def __ge__(self, other_date):
    return self.to_gregorian() >= other_date                
def __le__(self, other_date):
    return self.to_gregorian() <= other_date    
```

AlternateDate objects can get in on the fun as well. I defined the magic methods that control date objects' response to the comparison, addition and subtraction methods—and what's great about Python's operator handling is, I'm able to do it with respect to the operators themselves. In each case it's usually just a matter of presenting the AlternateDate's Gregorian equivalent back to the object it's being compared to. Thus if you want to compare a Positivist date and a Gregorian date, the Positivist date will respond with a standard datetime version of itself, and the two datetime date objects will be able to report their comparison. But more exciting is that if you want to compare to Positivist dates, the exact same behavior produces the desired outcome: the operator is passed to the first of the two dates, which produces a Gregorian version of itself and passes control back to the other date. The other date does the exact same thing, producing a Gregorian version of itself and calling the operator with its argument. Now the comparison is being called on two standard datetime `date` objects and the correct result is returned.

```python
>>> print(d)
Friday, 26th of Archimedes, 225: Frontinus
>>> print(d1)
Saturday, 27th of Saint Paul, 231: William Penn
>>> d1 > d
True
>>> d1 - d
datetime.timedelta(2248)
>>> d1 + datetime.timedelta(2248)
positivist date(237, 8, 27)
>>> d - datetime.date.today()
datetime.timedelta(2)
```

## Calendar Metaprogramming

As we saw above, many of the specifics of the Positivist Calendar are just design decisions, finding a comfortable number of weeks per month and months per year to fit neatly over a seven-day week and within a 365-day year. But with the introduction of the epagomenal days, Comte could have chosen any arrangement to fill out the solar year. Even the number of weeks in a day is somewhat arbitrary. 

So once I established the mechanics of creating date objects I decided I'd like to parameterize all these design decisions and make a flexible `AlternateCal` class that could create date objects under any calendar reform scheme that the user might like.

```python
def __init__(calendar, w_i_month=4, d_i_week=7, year_1=1789):
        calendar.days_in_a_month = d_i_week * w_i_month
        calendar.days_in_a_week = d_i_week
        calendar.weeks_in_a_month = w_i_month
        calendar.months_in_a_year = 365 // calendar.days_in_a_month
        calendar.epagomenal_days = 365 % calendar.days_in_a_month
        name_choices = ('New Adjusted', 'Utilitarian', 'Lycurgian', 'Multi-Manifold', 'Positivist', 'Crepuscular', 'Urquhart', 'Adamantine', 'Organic Non-Repeating', 'Antediluvian', 'Re-Corresponding')
        calendar.name = name_choices[hash((w_i_month, d_i_week, year_1)) % 11]
        calendar.year_offset = year_1
```

To this end I created the `AlternateCal` class as a parent class, which takes a few parameters as to how the weeks and months should fit together. 

```python
>>> print(AlternateCal(w_i_month=14, d_i_week=3, year_1=400))
The Organic Non-Repeating calendar, consisting of 3-day weeks, 14-week months, and 8-month years, with 29 epagomenal day(s).
>>> print(AlternateCal(w_i_month=5, d_i_week=6, year_1=11000))
The Utilitarian calendar, consisting of 6-day weeks, 5-week months, and 12-month years, with 5 epagomenal day(s).
```

The `alternateDate` class is then defined within the AlternateCal's `__init__` function. All of its own logic—for instance, which day of the year it is (very useful for doing conversions to other calendars), is performed relative to the parameters set by its parent class.

```python
self.day = day
self.day_of_year = (month - 1) * calendar.weeks_in_a_month * calendar.days_in_a_week + day
```

In this way we can create objects that are instances of the same `AlternateDate` class, but initialized with fundamentally different behavior.

```python
>>> posical = AlternateCal(w_i_month=4, d_i_week = 7, year_1 = 1789)
>>> badcal = AlternateCal(w_i_month=6, d_i_week = 8, year_1 = 1945)
>>> print(posical)
The Positivist calendar, consisting of 7-day weeks, 4-week months, and 13-month years, with 1 epagomenal day(s).
>>> print(badcal)
The Re-Corresponding calendar, consisting of 8-day weeks, 6-week months, and 7-month years, with 29 epagomenal day(s).
>>> d1 = posical.date(200, 4, 15)
>>> d2 = badcal.date(54, 1, 3)
>>> d1
positivist date(200, 4, 15)
>>> d2
re-corresponding date(54, 1, 3)
>>> d2 - d1
datetime.timedelta(3556)
>>> d2 + datetime.timedelta(3556)
re-corresponding date(63, 6, 32)
>>> d1 + datetime.timedelta(3556)
positivist date(210, 1, 3)
```

# Right History's Wrongs!

I encourage you to [play around](http://github.com/subsetpark/posical) with this library and make your own improbable calendar reforms. There's also a [barebones web app](http://posical.herokuapp.com), to which I will be adding more of the Python library's features.