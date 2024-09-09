import re

def split_and_indent(text):
    # Define the keywords to split on
    keywords = ["let", "then", "=>"]
    # Create a regex pattern to match any of the keywords
    pattern = re.compile(r'\b(' + '|'.join(keywords) + r')\b')
    
    # Split the text using the pattern
    segments = pattern.split(text)
    
    # Iterate over the segments and print them with appropriate indentation
    for i, segment in enumerate(segments):
        if segment in keywords:
            print(segment)
        else:
            # Determine if the previous segment was a keyword that requires indentation
            if i > 0 and segments[i-1] in ["let", "then", "=>"]:
                print("    " + segment.strip())
            else:
                print(segment.strip())

# Example usage
long_line = "let TableB = fun (arg_TableB) => if and(arg_TableB.Dish == 'Spareribs,arg_TableB.Dessert == 'Cake,arg_TableB.Children == false) then {Beverages = 'beer} else if arg_TableB.Dish == 'Stew && arg_TableB.Children == false then {Beverages = 'wine} else if arg_TableB.Dish == 'Roastbeef && arg_TableB.Children == false then {Beverages = 'soda} else if and(arg_TableB.Dish == 'Steak) then {Beverages = 'lemonade} else {Beverages = '`apple juice`} in let TableA = fun (arg_TableA) => if and(arg_TableA.season == 'Fall,arg_TableA.guests <= 8,arg_TableA.veg_guests == false) then {Dish = 'Spareribs,Dessert = 'Cake} else if and(arg_TableA.season == 'Winter,arg_TableA.guests <= 8,arg_TableA.veg_guests == false) then {Dish = 'Roastbeef,Dessert = 'Brownie} else if and(arg_TableA.season == 'Spring,arg_TableA.guests <= 4,arg_TableA.veg_guests == false) then {Dish = '`Fancy Steak`,Dessert = 'Pie} else if (5 >= arg_TableA && arg_TableA <= 8) && arg_TableA.veg_guests == false then {Dish = 'Stew,Dessert = 'Pie} else if and(arg_TableA.season == 'Summer,arg_TableA.guests <= 8,arg_TableA.veg_guests == false) then {Dish = '`Light salad and a nice steak`,Dessert = '`Ice cream`} else if arg_TableA.guests > 8 && arg_TableA.veg_guests == false then {Dish = 'Stew,Dessert = 'Cake} else {Dish = 'Pasta,Dessert = 'Cake} in let r0 = TableB({Dish = dish,Dessert = dessert,Children = false}) in let beverage = r0.Beverages in TableA({season = 'Fall,guests = 5,veg_guests = true})"

split_and_indent(long_line)