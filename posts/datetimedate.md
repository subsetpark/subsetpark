title: Comparing Dates and Datetimes in the Django ORM
date: 2015-04-17
status: post

## The issue at hand 

When working in Django, one often finds oneself with the following sort of question:

*How many records were created (or updated, or posted) today (or yesterday, or last Monday)?*

But most of the time, your `created_on`/`updated_on`/`posted_on` column will be a Datetime, and today is not a Datetime—it's a Date[^1]. So if you simply query like this:

    Foo.objects.filter(created_on=date)

You'll always receive an empty result—or at best you'll receive only those objects that were created at the exact moment that comes up when `date` is coerced into a Datetime (in the case of right now, at 12:21 EST on April 17, 2015, that would be `2015-04-17 04:00:00`). 

[^1]: The difference being that Datetimes refer to a specific time on a specific date—the difference between `2015-04-14 16:07:51` and `2015-04-14`.

It turns out this question tends to come up a lot. Here's the top four results in a google search for [django compare by date of datetime][gs]:

1. [Compare date and datetime in Django][gs0]
2. [Using datetime to compare with dates in Django][gs1]
3. [How can I filter a date of a DateTimeField in Django?][gs2]
4. [python - How to compare dates in Django][gs3]
5. [Comparing a DateTimeField to a date is too hard][gs4]

Those 4 Stack Overflow posts also give us an idea of the state of the art in addressing this common need.

[gs]: https://www.google.com/search?q=django+compare+by+date+of+datetime
[gs0]: http://stackoverflow.com/questions/10048216/compare-date-and-datetime-in-django
[gs1]: http://stackoverflow.com/questions/4606207/using-datetime-to-compare-with-dates-in-django
[gs2]: http://stackoverflow.com/questions/1317714/how-can-i-filter-a-date-of-a-datetimefield-in-django
[gs3]: http://stackoverflow.com/questions/3798812/how-to-compare-dates-in-django
[gs4]: https://code.djangoproject.com/ticket/9596

To sum up, here are the approaches recommended in the many answers to these similar questions:

### Use __range
Picking a min and max datetime, pass them as a tuple like this: 

    Foo.objects.filter(created_on__range=(min_dt, max_dt))

Which will return all datetime records within the two values of the tuple.

There are a couple ways to get your min and max. If you have a target datetime already you can use timedeltas:

    min_dt = target_date - timedelta(hours=12)
    max_dt = target_date + timedelta(hours=12)

Or if you have a target date you can use the `min` and `max` of that date:

    min_dt = datetime.datetime.combine(date, datetime.time.min)
    max_dt = datetime.datetime.combine(date, datetime.time.max)

These of course have slightly different effects. In particular, you need to decide whether you're looking for everything on a certain date, or everything within a certain 24-hour window. Usually, at [Makespace][], it's the former.

[Makespace]: https://makespace.com/about/careers/

### Use __contains
Take advantage of SQL's string matching by filtering like this:

    Foo.objects.filter(created_on__contains=date)

Neither of the above two really knocks it out of the park for me. The latter is, generously, a hack; taking advantage of the way that dates and datetimes are stored as strings in order to to find matches. But the former is quite verbose, and requires manually doing tricky datetime and timedelta logic.

At [Makespace][] we opted for the simplicity of the latter. However, the writing was on the wall.

This week, it came time to update a bunch of our system packages, including [PyMySQL](https://pypi.python.org/pypi/PyMySQL). With the latest version of PyMySQL, running a `contains` lookup on a datetime with a date starts, rather sensibly, to throw an error: `Incorrect datetime value: '%2015-04-12%' for column 'created_on' at row 1`. That is: PyMySQL no longer likes to pass a date value to to comparison with a datetime field, even while using `contains`. It was the end of the road for `contains`. 

Luckily, [Django 1.7][d17] introduced some new features when it comes to enxtending the lookup system of the ORM, meaning that a more elegant and efficient solution is within reach, without having to wait for the six year-old [#9596][gs4] to get merged. 

[d17]: https://docs.djangoproject.com/en/1.7/releases/1.7/

## django.db.models.Transform

The new `Transform` class, as it says in the 1.7 release notes, "allows transformations of database values prior to the final lookup". While it looks a bit intimidating, we were able to use the [howto documentation](https://docs.djangoproject.com/en/1.7/howto/custom-lookups/) to write a Transform that does exactly what we've been looking for within our filter calls. It looks like this:

    from django.db.models import Transform
    from django.db import models

    class MySQLDatetimeDate(Transform):
        """
        This implements a custom SQL lookup when using `__date` with datetimes.
        To enable filtering on datetimes that fall on a given date, import
        this transform and register it with the DateTimeField.
        """
        lookup_name = 'date'

        def as_sql(self, compiler, connection):
            lhs, params = compiler.compile(self.lhs)
            return 'DATE({})'.format(lhs), params

        @property
        def output_field(self):
            return models.DateField()

A Transform is a very useful kind of lookup (the things separated by `__` in the string `foo__bar__date` are chained lookups) which takes the left-hand value (in this case, `bar`) and applies a further *transformation* to it before the query is actually run. In this case, `bar` is a SQL datetime field that we want to use the lookup `date` to transform into a date field.

This allows us to make queries like this:

    Foo.objects.filter(created_on__date=date)

And get back everything that was created on the date in question. 

The important bits are at lines 14 and 18. On line 14, in the `as_sql()` method, we've already gotten the SQL column that we'll be comparing against `date`, and now we're building our raw SQL query. All we do here is wrap the column data in a SQL [`DATE()`](http://www.w3schools.com/sql/func_date.asp) call, which returns the date element of a datetime expression. Then, on line 18, we specify that the column data, after the transform, should be treated as a DateField (and no longer a DateTimeField)—this allows the db to perform the comparison without thinking that it has to coerce the right-hand data into a Datetime in order to make a comparison.

Of course, this implementation relies on your particular flavor of SQL *having* a `DATE()` function. MySQL does. So does [SQLite](http://www.sqlite.org/lang_datefunc.html). On the other hand, I haven't worked with PostgreSQL personally, but some googling leads me to believe that it does *not* have a `DATE()` function. So an implementation this simple seems like it will necessarily be somewhat backend-dependent.

Nevertheless, after you define this transform, all you have to do is register it to the DateTimeField you use with `register_lookup`. Then you can use the `__date` lookup transparently in your chaining filter queries.

For me, this is a resolution of a long-standing hack that I've had in place when working with Django. It's idiomatic within both Django and SQL (for certain flavors of SQL), and both terse and obvious in application.
