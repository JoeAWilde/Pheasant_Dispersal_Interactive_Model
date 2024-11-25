import tkinter as tk
from tkinter import filedialog
import rasterio
import numpy as np
from PIL import Image, ImageTk

# Habitat types and their corresponding colors
habitats = [
    "Deciduous woodland", "Coniferous woodland", "Arable", "Improved grassland", 
    "Neutral grassland", "Calcareous grassland", "Acid grassland", "Fen, marsh & swamp", 
    "Heather", "Heather grassland", "Bog", "Inland rock", "Saltwater", "Freshwater", 
    "Supralittoral rock", "Supralittoral sediment", "Littoral rock", "Littoral sediment", 
    "Saltmarsh", "Urban", "Suburban"
]

# RGB color codes for habitats
red = [255, 0, 115, 0, 127, 112, 153, 255, 128, 230, 0, 210, 0, 0, 204, 204, 255, 255, 128, 0, 128]
green = [0, 102, 38, 255, 229, 168, 129, 255, 26, 140, 128, 210, 0, 0, 179, 179, 255, 255, 128, 0, 128]
blue = [0, 0, 0, 0, 127, 0, 0, 0, 128, 166, 115, 255, 128, 255, 0, 0, 128, 128, 255, 0, 128]

# Create RGB colors
colors = [
    f'#{r:02x}{g:02x}{b:02x}' for r, g, b in zip(red, green, blue)
]

# Map habitat colors to raster values
color_mapping = {color: i + 1 for i, color in enumerate(colors)}  # Raster values start from 1
reverse_color_mapping = {v: k for k, v in color_mapping.items()}
habitat_mapping = {color: habitats[i] for i, color in enumerate(colors)}

def load_raster(filepath):
    global raster_data, img_shape, tk_image, raster_image
    try:
        with rasterio.open(filepath) as dataset:
            # Read raster data and store for editing
            raster_data = dataset.read(1).copy()
            img_shape = raster_data.shape

            # Map raster values to colors for display
            display_data = np.zeros((img_shape[0], img_shape[1], 3), dtype=np.uint8)
            for value, color in reverse_color_mapping.items():
                mask = raster_data == value
                rgb = tuple(int(color[i:i+2], 16) for i in (1, 3, 5))
                display_data[mask] = rgb

            # Convert to image
            img = Image.fromarray(display_data)
            raster_image = img  # Keep the original image for updates
            tk_image = ImageTk.PhotoImage(img)

            # Display raster on canvas, centered
            canvas.delete("all")  # Clear the canvas
            canvas.create_image(canvas.winfo_width() // 2, canvas.winfo_height() // 2, anchor="center", image=tk_image)
            canvas.image = tk_image
            status_label.set(f"Loaded raster: {filepath}")
    except Exception as e:
        status_label.set(f"Error loading raster: {e}")

def on_canvas_drag(event):
    if selected_color is None:
        status_label.set("No color selected.")
        return

    # Get the canvas dimensions and the image placement
    canvas_width = canvas.winfo_width()
    canvas_height = canvas.winfo_height()
    raster_width, raster_height = img_shape[1], img_shape[0]

    # Calculate offsets for centering
    offset_x = (canvas_width - raster_width) // 2
    offset_y = (canvas_height - raster_height) // 2

    # Get the pixel coordinates relative to the raster
    raster_x = event.x - offset_x
    raster_y = event.y - offset_y

    if 0 <= raster_x < raster_width and 0 <= raster_y < raster_height:
        # Update raster data within brush size
        new_value = color_mapping[selected_color]
        for dx in range(-brush_size, brush_size + 1):
            for dy in range(-brush_size, brush_size + 1):
                new_x, new_y = raster_x + dx, raster_y + dy
                if 0 <= new_x < raster_width and 0 <= new_y < raster_height:
                    raster_data[new_y, new_x] = new_value

        # Refresh the displayed raster
        update_display()

def update_display():
    global tk_image
    display_data = np.zeros((img_shape[0], img_shape[1], 3), dtype=np.uint8)
    for value, color in reverse_color_mapping.items():
        mask = raster_data == value
        rgb = tuple(int(color[i:i+2], 16) for i in (1, 3, 5))
        display_data[mask] = rgb

    # Update the image
    img = Image.fromarray(display_data)
    raster_image.paste(img)
    tk_image = ImageTk.PhotoImage(raster_image)
    canvas.create_image(canvas.winfo_width() // 2, canvas.winfo_height() // 2, anchor="center", image=tk_image)
    canvas.image = tk_image

def save_raster():
    save_path = filedialog.asksaveasfilename(defaultextension=".tif", filetypes=[("GeoTIFF files", "*.tif")])
    if save_path:
        with rasterio.open(filepath) as src:
            profile = src.profile
            with rasterio.open(save_path, 'w', **profile) as dst:
                dst.write(raster_data, 1)
        status_label.set(f"Raster saved to {save_path}")

def select_color(color):
    global selected_color
    selected_color = color
    status_label.set(f"Selected color: {habitat_mapping[color]}")

# Initialize Tkinter root
root = tk.Tk()
root.title("Raster Editor")

# Fullscreen adaptation
screen_width = root.winfo_screenwidth()
screen_height = root.winfo_screenheight()
root.geometry(f"{screen_width}x{screen_height}")

# Variables
raster_data = None
selected_color = None
img_shape = None
raster_image = None
filepath = r"..\Data\UKCEH-2018-25m_AllUK\example_hab.tif"

# Brush size for drag painting
brush_size = 5  # Adjust this value for a larger brush

# Status label
status_label = tk.StringVar()
status_label.set("No raster loaded yet.")
tk.Label(root, textvariable=status_label).pack()

# Save button at the top
save_button = tk.Button(root, text="Save Raster", command=save_raster)
save_button.pack(pady=10)

# Canvas for raster
canvas = tk.Canvas(root, bg="white", width=int(screen_width * 0.8), height=int(screen_height * 0.6))
canvas.pack()
canvas.bind("<B1-Motion>", on_canvas_drag)

# Habitat buttons in 5 columns
color_frame = tk.Frame(root)
color_frame.pack()

num_columns = 5
for i, (color, habitat) in enumerate(zip(colors, habitats)):
    row = i // num_columns
    col = i % num_columns
    frame = tk.Frame(color_frame)
    frame.grid(row=row, column=col, padx=5, pady=5, sticky='w')
    btn = tk.Button(frame, bg=color, width=3, height=1, command=lambda c=color: select_color(c))
    btn.pack(side=tk.LEFT)
    tk.Label(frame, text=habitat).pack(side=tk.LEFT)

# Load the raster
load_raster(filepath)

# Run the app
root.mainloop()
