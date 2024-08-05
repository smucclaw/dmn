
def opinion(stage, sector, stage_com, has_ESG, wants_ESG):
    if stage == 'Seed' and sector == 'Information Technology' and stage_com == 'Pre-Revenue' :
        return 'Interesting'
    elif stage == 'Series A' and sector == 'Information Technology' and stage_com == 'Pre-Profit' :
        return 'Interesting'
    elif has_ESG == True and wants_ESG == True :
        return 'Interesting'
    else:
        return 'reject'
    

print (opinion('Seed', 'Information Technology', 'Pre-Profit', True, True)) 

# actual output
def get_opinion ( stage, sector, stage_com, has_ESG, wants_ESG ):
    if stage == 'Seed' and sector == 'Information Technology' and stage_com == 'Pre-Revenue' :
        return 'Interesting'
    else:
        if stage == 'Series A' and sector == 'Information Technology' and stage_com == 'Pre-Profit' :
            return 'Interesting'
        else:
            if has_ESG == True and wants_ESG == True :
                return 'Interesting'
            else:
                return 'reject'
            
print (get_opinion('Seed', 'Information Technology', 'Pre-Profit', True, True))