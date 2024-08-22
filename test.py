def opinion ( stage, sector, stage_com, has_esg, wants_esg ):
    if stage == 'Seed' and sector == 'Information Technology' and stage_com == 'Pre-Revenue' :
        return 'interesting'
    else:
        if stage == 'Series A' and sector == 'Information Technology' and stage_com == 'Pre-Profit' :
            return 'interesting'
        else:
            if has_esg == True and wants_esg == True :
                return 'interesting'
            else:
                return 'reject'
            
print(opinion('Seed', 'Information Technology', 'Pre-Revenue', False, False)) # interesting