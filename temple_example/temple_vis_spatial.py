# %%
import polars as pl
import plotly.express as px
import pyarrow.parquet as pq
# %%
dat = pl.from_arrow(pq.read_table("temple_details.parquet"))


# %%
dat.filter(pl.col("stateRegion") == "Idaho")


# %%
px.scatter_geo(dat.filter(pl.col("country") == "United States"), lat="lat", lon="long", hover_name="location").update_layout(
        title = 'LDS Temple Locations',
        geo_scope='usa')
       

# %%
