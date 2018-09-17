import pandas as pd 
import sqlite3 as sq


conn = sq.connect("./Heartstone/database.sqlite")
query = """Select player_class, 
			count(*) 
			from cards as c
			inner join mechanics as m
			on c.card_id = m.card_id
			where player_class <> 'NEUTRAL'
			and mechanic like '%TAUNT%'
			group by player_class
			;"""

			
cards = pd.read_sql_query(query, conn)

print(cards)