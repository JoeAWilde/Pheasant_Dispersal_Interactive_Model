import streamlit as st
from streamlit_folium import st_folium
import folium
import pandas as pd

# Initialize session state variables for coordinates, map center, and zoom level
if "coords" not in st.session_state:
    st.session_state["coords"] = []

if "map_center" not in st.session_state:
    st.session_state["map_center"] = [55.3781, -3.4360]  # Default to UK center

if "zoom" not in st.session_state:
    st.session_state["zoom"] = 5  # Default zoom level

if "mode" not in st.session_state:
    st.session_state["mode"] = "Release Site"  # Start with release sites

# Toggle mode with "Next" button to switch between release and feeder
if st.button("Next"):
    st.session_state["mode"] = "Feeder" if st.session_state["mode"] == "Release Site" else "Release Site"

# Create the Folium map with the current center and zoom
m = folium.Map(location=st.session_state["map_center"], zoom_start=st.session_state["zoom"], control_scale=True)

# Display instructions with current mode
st.write(f"Click on the map to record coordinates for: **{st.session_state['mode']}**.")

# Add markers for each recorded coordinate, with color-coded popups
for coord in st.session_state["coords"]:
    color = "blue" if coord[2] == "Release Site" else "red"
    folium.Marker(
        [coord[0], coord[1]],
        popup=f"{coord[2]} - Lat: {coord[0]}, Lng: {coord[1]}",
        icon=folium.Icon(color=color)
    ).add_to(m)

# Render map and capture click data
map_data = st_folium(m, width=700, height=500)

# If the map is clicked, record the point with the current mode
if map_data and map_data.get("last_clicked"):
    lat = map_data["last_clicked"]["lat"]
    lng = map_data["last_clicked"]["lng"]

    # Append the clicked point with the current mode
    st.session_state["coords"].append((lat, lng, st.session_state["mode"]))

    # Update map center and zoom level for consistency across clicks
    st.session_state["map_center"] = [lat, lng]
    st.session_state["zoom"] = 15  # Fixed zoom on each click (optional)

# Display recorded coordinates
st.write("Coordinates Recorded So Far:")
if st.session_state["coords"]:
    for coord in st.session_state["coords"]:
        st.write(f"{coord[2]} - Lat: {coord[0]}, Lng: {coord[1]}")

# Button to save coordinates to a CSV file
if st.button("Save coordinates"):
    df = pd.DataFrame(st.session_state["coords"], columns=["Latitude", "Longitude", "Type"])
    df.to_csv("coord_files/release_coords.csv", index=False)
    st.success("Coordinates saved!")
