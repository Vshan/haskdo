import plotly
import plotly.graph_objs as go
from datetime import date, timedelta

def get_nums_from_date(date):
	date_string = date.strftime("%d-%m-%Y")
	filename = "../data/" + date_string + "plotdata.txt"
	lines = [line.rstrip('\n') for line in open(filename)]
	nums = lines[0].rsplit(',')[:-1]
	return map(float,nums)

today_date = date.today().strftime("%d-%m-%Y")
yesterday_date = (date.today() - timedelta(1)).strftime("%d-%m-%Y")

today_mins = get_nums_from_date(date.today())
yesterday_mins = get_nums_from_date(date.today() - timedelta(1))

sum = 0
for i in today_mins:
	sum += i

perc = []
for i in today_mins:
	perc.append((i / sum) * 100)

fig = {
    'data': [{'labels': ['Study', 'Code', 'Read', 'Write', 'Hygiene', 'Exercise', 'Class', 'ClassWork', 'Food', 'Fun', 'Sleep'],
              'values': perc,
              'type': 'pie'}],
    'layout': {'title': 'Daily Report'}
}

plotly.offline.plot(fig, filename='Pie Chart Example')

trace0 = go.Bar(
    x=['Study', 'Code', 'Read', 'Write', 'Hygiene', 'Exercise', 'Class', 'ClassWork', 'Food', 'Fun', 'Sleep'],
    y=map(int, today_mins),
    name='Today',
    marker=dict(
        color='rgb(49,130,189)'
    )
)
trace1 = go.Bar(
    x=['Study', 'Code', 'Read', 'Write', 'Hygiene', 'Exercise', 'Class', 'ClassWork', 'Food', 'Fun', 'Sleep'],
    y=map(int, yesterday_mins),
    name='Yesterday',
    marker=dict(
        color='rgb(204,204,204)',
    )
)
data = [trace0, trace1]
layout = go.Layout(
    xaxis=dict(
        # set x-axis' labels direction at 45 degree angle
        tickangle=-45,
    ),
    barmode='group',
)
fig = go.Figure(data=data, layout=layout)
plotly.offline.plot(fig, filename='angled-text-bar')