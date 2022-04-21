import requests
from bs4 import BeautifulSoup
from lxml import etree
import pandas as pd
import os
import time

CURDIR = './'
BASE_URL = 'https://en.volleyballworld.com'

TEAM_TO_ABBR = {'china': 'chn', 'belgium': 'bel', 'brazil': 'bra', 
                'bulgaria': 'bul', 'dominican republic': 'dom', 
                'germany': 'ger', 'italy': 'ita', 'japan': 'jpn', 
                'korea': 'kor', 'netherlands': 'ned', 'poland': 'pol', 
                'russia': 'rus',  'serbia': 'srb', 'thailand': 'tha',
                'turkey': 'tur', 'usa': 'usa'}


def format_snake(c):
    """
    Helper function for turning colnames into snake format
    """
    c_list = c.split(' ')
    # Change to lower case
    c_list = [w.lower() for w in c_list]
    return '_'.join(c_list)


def create_unique_strings(org_l):
    """
    Add prefix to the string if the strings in the the list are duplicates.
    """
    l = []
    for name in org_l:
        if name not in l:
            l.append(name)
            continue
        count = 0
        while  str(count) + '/' + name in l:
            count += 1
        l.append(str(count) + '/' + name)
    return l


def save_csv(df, filename):
    """
    Save the dataframe as a csv file.
    """
    fname = os.path.join(CURDIR, filename)
    df.to_csv(fname, index=False)
    print(fname + ' saved.')
    return True


def retrieve_first_table(url, table_idx=0, header_span=False, th_row=0, 
                         prefix_col=0, override_column=None, td_span=False):
  """
  Scrape the first table on the page at url.
  """
  data = requests.get(url).text
  soup = BeautifulSoup(data, "lxml")

  # Retrive table
  table = soup.find_all("table")[table_idx]
  
  # Find column names
  if override_column is None:
    header = table.thead.find_all('tr')[th_row].find_all('th')
    if header_span:
      colnames = [th.find_all('span')[0].text for th in header]
    else:
      colnames = [th.text for th in header]
    colnames = ['']*prefix_col + colnames
    # Change column names into snake format
    colnames = [format_snake(c) for c in colnames]
    # Make sure the column names are unique
    colnames = create_unique_strings(colnames)
  else:
    colnames = override_column
  
  df = pd.DataFrame(columns=colnames)

  # Collecte table data
  for tr in table.tbody.find_all('tr'):    
      # Find all data for each column
      columns = tr.find_all('td')
      if columns == []:
        continue
      if td_span:
        row_dict = {}
        for i in range(len(columns)):
          spans = columns[i].find_all('span')
          if spans:
            value = '-'.join([span.text.strip() for span in spans])
          else:
            value = columns[i].text.strip()
          row_dict[colnames[i]] = value
      else:
          row_dict = {colnames[i]: columns[i].text.strip() for i in range(len(columns))}
      df = df.append(row_dict, ignore_index=True)
  return df


def get_match_summary():
    """
    Get the match summary for the round robin.
    """
    match_summary_url = "https://en.volleyballworld.com/en/vnl/2019/women/resultsandranking/round1"
    colnames = ['number', 'date', 'teams', 'sets', 'set1_point', 'set2_point', 
                'set3_point', 'set4_point', 'set5_point', 
                'pionts', 'time', 'audience']
    match_summary_df = retrieve_first_table(match_summary_url, override_column=colnames, td_span=True, table_idx=1)
    for i in range(1, 6):
        # Clean the set point columns
        match_summary_df['set%s_point' % i] = match_summary_df['set%s_point' % i]\
            .apply(lambda x: (x[:2] if x[1].isnumeric() else x[0]) + '-' + 
                (x[-2:] if x[-2].isnumeric() else x[-1]) if x!='-' else x)
    return match_summary_df


def save_best_players():
    """
    Save 7 csv files for the best player data.
    """
    positions = ['best-scorers', 'best-spikers', 'best-blockers', 'best-servers', 'best-setters', 'best-diggers', 'best-receivers']
    base_url_best_players = "https://en.volleyballworld.com/en/vnl/2019/women/statistics/"

    for i in range(7):
        df = retrieve_first_table(base_url_best_players, table_idx=i, header_span=False)
        # Drop the last row
        print(df.tail(1))
        df.drop(df.tail(1).index,inplace=True)
        # Insert a rank column
        df['rank'] = list(range(1, len(df)+1))
        save_csv(df, positions[i] + '.csv')
        
    return df


def get_team_rank_with_match():
    """
    Save the team rank csv file.
    """
    # Scrape table from the webpage
    team_rank_match_url = 'https://en.volleyballworld.com/en/vnl/2019/women/resultsandranking/round1'
    team_rank_match_df = retrieve_first_table(team_rank_match_url, 
                                            header_span=False, th_row=1, prefix_col=3)
    # Drop empty column
    team_rank_match_df.drop(team_rank_match_df.columns[2], axis=1, inplace=True)

    # Set column names
    colnames = team_rank_match_df.columns
    suffix_replace_dict = {'0/': 'set_', '1/': 'point_'}
    for old in suffix_replace_dict:
        new = suffix_replace_dict[old]
        colnames = [name.replace(old, new) for name in colnames]

    # Rename columns
    colnames[:5] = ['rank', 'team_full', 'match_total', 'match_win', 'match_lose']
    colnames[-1] = 'point_ratio'
    colnames[-4] = 'set_ratio'
    team_rank_match_df.columns = colnames

    # Insert column for team abbreviation
    team_rank_match_df['team'] = team_rank_match_df['team_full'].apply(lambda x: TEAM_TO_ABBR[x.lower()].upper())

    return team_rank_match_df

def get_player_bio_df():
    """
    Save player bio into a csv file.
    """
    player_all_df = pd.DataFrame()
    for team in TEAM_TO_ABBR:
        abbr = TEAM_TO_ABBR[team]
        link_part = '%s-%s' % (abbr, team)
        player_url = "https://en.volleyballworld.com/en/vnl/2019/women/teams/%s/team_roster" % link_part
        player_df = retrieve_first_table(player_url, header_span=False)
        # Drop index
        player_df.drop(player_df.columns[0], axis=1, inplace=True)
        # Insert a team column
        player_df['team'] = abbr.upper()
        player_all_df = pd.concat([player_all_df, player_df])

    return player_all_df


def get_position(url):
    """
    Get info about player position from url.
    """
    data = requests.get(url).text
    soup = BeautifulSoup(data, "lxml")
    position = soup.find_all("div", class_='col-1-3')[0].find_all('li')[0].find_all('span')[1].text.strip()
    return position


def get_player_href(url):
    """
    Get the list of links for each player page.
    """
    data = requests.get(url).text
    soup = BeautifulSoup(data, "lxml")
    table = soup.find_all('table')[0]
    a_s = table.find_all('a', href=True)
    hrefs = [BASE_URL + a.get('href') for a in a_s]
    return hrefs


def get_player_bio_df():
    """
    Save player bio into a csv file.
    """
    player_all_df = pd.DataFrame()
    for team in TEAM_TO_ABBR:
        abbr = TEAM_TO_ABBR[team]
        link_part = '%s-%s' % (abbr, team)

        team_url = "https://en.volleyballworld.com/en/vnl/2019/women/teams/%s/team_roster" % link_part
        player_df = retrieve_first_table(team_url, header_span=False)
        # Drop index
        player_df.drop(player_df.columns[0], axis=1, inplace=True)
        # Insert a team column
        player_df['team'] = abbr.upper()

        # Get player position
        player_links = get_player_href(team_url)
        positions = [get_position(url) for url in player_links]
        player_df['position'] = positions

        player_all_df = pd.concat([player_all_df, player_df])

    return player_all_df



def get_vnl_schedule_2021()
    """ 
    Query for the 2021 match schedule and overall result.
    """
    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0',}
    url = 'https://en.volleyballworld.com/volleyball/competitions/vnl-2021/schedule/'
    html = requests.get(url,headers = headers)
    selector = etree.HTML(html.text)
    infos = selector.xpath("//div[@class='vbw-mu--match vbw-mu-finished vbw-mu']")#/a/div/div[@class='vbw-mu__info--details']/text()
    infoslist = []
    titlelist =['matchid','matchname','teamhome','teamaway','scorehome','awayhome','result']
    for info in infos:
        matchid = info.xpath("@matchid")[0][-5:]
        matchname = info.xpath("a/div/div[@class='vbw-mu__info--details']/text()")[0]
        teamhome = info.xpath("a/div/div/div[@class='vbw-mu__team__name vbw-mu__team__name--abbr']/text()")[0]
        teamaway = info.xpath("a/div/div/div[@class='vbw-mu__team__name vbw-mu__team__name--abbr']/text()")[1]
        scorehome = info.xpath("a/div/div/div/div[contains(@class,'vbw-mu__score--home')]/text()")[0]
        awayhome = info.xpath("a/div/div/div/div[contains(@class,'vbw-mu__score--away')]/text()")[0]
        result = info.xpath("a/div/div[@class='vbw-mu__sets--result']/span/text()")
        infolist = []
        infolist.append(matchid)
        infolist.append(matchname)
        infolist.append(teamhome)
        infolist.append(teamaway)
        infolist.append(scorehome)
        infolist.append(awayhome)
        infolist.append(result)
        infoslist.append(infolist)
    schedule = pd.DataFrame(infoslist,columns=titlelist)    
    matchid_list = list(schedule['matchid'])
    schedule["result"] = schedule["result"].apply(lambda x: x.replace("'", ""))
    schedule["result"] = schedule["result"].apply(lambda x: x[1:-1].split(',')) #
    schedule["result"] = schedule["result"].apply(lambda x: [int(i.strip()) for i in x if i != ' -'])
    schedule["points_home"] = schedule["result"].apply(lambda x: sum([x[i] for i in range(len(x)) if i % 2 == 0]))
    schedule["points_away"] = schedule["result"].apply(lambda x: sum([x[i] for i in range(len(x)) if i % 2 == 1]))
    schedule.to_csv('schedule2021.csv', index=False)
    return matchid_list

def get_one_match_data(matchid):
    """
    Query for one match given the ID.
    """
    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0',}
    url ='https://en.volleyballworld.com/volleyball/competitions/vnl-2021/schedule/' + matchid + '/_libraries/_finished-match'
          
    html = requests.get(url,headers = headers)
    selector = etree.HTML(html.text)
    nationinfos = selector.xpath("//section/div/div/div/div/div/div/div/ul/li/a")
    dicinfo = {}
    for nationinfo in nationinfos:
        team = nationinfo.xpath('./@href')[0][1:]
        nation = nationinfo.xpath("./div[@class = 'vbw-mu__team__name vbw-mu__team__name--abbr']/text()")[0]
        dicinfo[team] = nation
    infoteam = selector.xpath("//table[contains(@class,'vbw-o-table vbw-match-player-statistic-table vbw-stats-scoring vbw-set-all')]")                                                 

    df = pd.DataFrame()                   
    infoslist = []
    titlelist =['schedule_id','nationality','number','name','position','attackpoints','blockpoints','servepoints','efficency',
                'totalabs','attack_pt','attack_err','attack_att','attack_tot','attack_eff','block_pt','block_err','block_touches','block_tot','block_eff',
    'serve_pt','serve_err','serve_attempts','serve_tot','serve_eff','reception_successful','reception_err',
    'reception_attempts','reception_tot','reception_eff','dig_digs','dig_err','dig_tot','dig_eff',
    'set_pt','set_err','set_attempts','set_tot','set_eff'] #'schedule_id','playerid','nationality','birthday','height','weight',
    
    
    for j in range(len(infoteam)):
        pageteam =     infoteam[j].xpath('./@data-team')[0]
        infos0 = selector.xpath("//table[contains(@class,'vbw-o-table vbw-match-player-statistic-table vbw-stats-scoring vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--scoring vbw-stats-player')]".format(pageteam))
        infos1 = selector.xpath("//table[contains(@class,'vbw-o-table vbw-match-player-statistic-table vbw-stats-attack vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--scoring vbw-stats-player')]".format(pageteam))
        infos2 = selector.xpath("//table[contains(@class, 'vbw-o-table vbw-match-player-statistic-table vbw-stats-block vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--attack vbw-stats-player')]".format(pageteam))
        infos3 = selector.xpath("//table[contains(@class, 'vbw-o-table vbw-match-player-statistic-table vbw-stats-serve vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--attack vbw-stats-player')]".format(pageteam))
        infos4 = selector.xpath("//table[contains(@class, 'vbw-o-table vbw-match-player-statistic-table vbw-stats-reception vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--attack vbw-stats-player')]".format(pageteam))
        infos5 = selector.xpath("//table[contains(@class, 'vbw-o-table vbw-match-player-statistic-table vbw-stats-dig vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--attack vbw-stats-player')]".format(pageteam))
        infos6 = selector.xpath("//table[contains(@class, 'vbw-o-table vbw-match-player-statistic-table vbw-stats-set vbw-set-all') and @data-team = '{}']/tbody/tr[contains(@class,'vbw-o-table__row vbw-o-table__row--attack vbw-stats-player')]".format(pageteam))
        for i in range(len(infos0)):
            # print(i,matchid)
            schedule_id = matchid
            nationality =  dicinfo[pageteam]
            number =       infos0[i].xpath("td[@class='vbw-o-table__cell shirtnumber']/text()")[0]
            name =         infos0[i].xpath("td[@class='vbw-o-table__cell playername']/text()")[0]
            position =     infos0[i].xpath("td[@class='vbw-o-table__cell position']/text()")[0]
            attackpoints = infos0[i].xpath("td[@class='vbw-o-table__cell attacks']/text()")[0]
            blockpoints =  infos0[i].xpath("td[@class='vbw-o-table__cell blocks']/text()")[0]
            servepoints =  infos0[i].xpath("td[@class='vbw-o-table__cell serves']/text()")[0]
            efficency =    infos0[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if efficency == []:
                efficency = [' ']
            efficency = efficency[0]    
            totalabs = infos0[i].xpath("td[@class='vbw-o-table__cell total-abs']/text()")[0]
            attack_pt =  infos1[i].xpath("td[@class='vbw-o-table__cell point']/text()")[0]
            attack_err = infos1[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0]
            attack_att = infos1[i].xpath("td[@class='vbw-o-table__cell attempts']/text()")[0]
            attack_tot = infos1[i].xpath("td[@class='vbw-o-table__cell total']/text()")[0]
            attack_eff = infos1[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if attack_eff == []:
                attack_eff = [' ']
            attack_eff = attack_eff[0]
            block_pt = infos2[i].xpath("td[@class='vbw-o-table__cell point']/text()")[0]
            block_err = infos2[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0] 
            block_touches = infos2[i].xpath("td[@class='vbw-o-table__cell touches']/text()")[0]
            block_tot = infos2[i].xpath("td[@class='vbw-o-table__cell total']/text()")[0]
            block_eff = infos2[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if block_eff == []:
                block_eff = [' ']
            block_eff = block_eff[0]
            serve_pt = infos3[i].xpath("td[@class='vbw-o-table__cell point']/text()")[0]
            serve_err = infos3[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0]  
            serve_attempts = infos3[i].xpath("td[@class='vbw-o-table__cell attempts']/text()")[0]
            serve_tot = infos3[i].xpath("td[@class='vbw-o-table__cell total']/text()")[0]
            serve_eff = infos3[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if serve_eff == []:
                serve_eff = [' ']
            serve_eff = serve_eff[0]    
            reception_successful = infos4[i].xpath("td[@class='vbw-o-table__cell successful']/text()")[0]
            reception_err = infos4[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0]      
            reception_attempts = infos4[i].xpath("td[@class='vbw-o-table__cell attempts']/text()")[0]
            reception_tot = infos4[i].xpath("td[@class='vbw-o-table__cell total']/text()")[0]
            reception_eff = infos4[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if reception_eff == []:
                reception_eff = [' ']
            reception_eff = reception_eff[0]    
            dig_digs = infos5[i].xpath("td[@class='vbw-o-table__cell digs']/text()")[0]
            dig_err = infos5[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0] 
            dig_tot = infos5[i].xpath("td[@class='vbw-o-table__cell attempts']/text()")[0]
            dig_eff = infos5[i].xpath("td[@class='vbw-o-table__cell total']/text()")
            if dig_eff == []:
                dig_eff = [' ']
            dig_eff = dig_eff[0]    
            set_pt = infos6[i].xpath("td[@class='vbw-o-table__cell successful']/text()")[0]
            set_err = infos6[i].xpath("td[@class='vbw-o-table__cell errors']/text()")[0]  
            set_attempts = infos6[i].xpath("td[@class='vbw-o-table__cell attempts']/text()")[0]
            set_tot = infos6[i].xpath("td[@class='vbw-o-table__cell total']/text()")[0]
            set_eff = infos6[i].xpath("td[@class='vbw-o-table__cell efficiency-percentage']/text()")
            if set_eff == []:
                set_eff = [' ']
            set_eff = set_eff[0]   
            infolist = []
            infolist.append(schedule_id)
            infolist.append(nationality)
            infolist.append(number)
            infolist.append(name)
            infolist.append(position)
            infolist.append(attackpoints)
            infolist.append(blockpoints)
            infolist.append(servepoints)
            infolist.append(efficency)
            infolist.append(totalabs)
            infolist.append(attack_pt)
            infolist.append(attack_err)
            infolist.append(attack_att)
            infolist.append(attack_tot)
            infolist.append(attack_eff)
            infolist.append(block_pt)
            infolist.append(block_err)
            infolist.append(block_touches)
            infolist.append(block_tot)
            infolist.append(block_eff)
            infolist.append(serve_pt)
            infolist.append(serve_err)
            infolist.append(serve_attempts)
            infolist.append(serve_tot)
            infolist.append(serve_eff)
            infolist.append(reception_successful)
            infolist.append(reception_err)
            infolist.append(reception_attempts)
            infolist.append(reception_tot)
            infolist.append(reception_eff)
            infolist.append(dig_digs)
            infolist.append(dig_err)
            infolist.append(dig_tot)
            infolist.append(dig_eff)
            infolist.append(set_pt)
            infolist.append(set_err)
            infolist.append(set_attempts)
            infolist.append(set_tot)
            infolist.append(set_eff)
            infoslist.append(infolist)
            time.sleep(0.5)
            # print(name,nationality,number)
    df = pd.DataFrame(infoslist,columns=titlelist)
    return df

def get_matches_data_2021(matchid_list)
    """
    Get detailed info for all matches.
    """
    totaldf = pd.DataFrame()
    for k in range(len(matchid_list)):
        print(k,matchid_list[k])
        dftemp = get_one_match_data(matchid_list[k])
        totaldf = pd.concat([totaldf,dftemp])
    totaldf.to_csv('match2021.csv', index=False) 
    return True

def main():
    player_df = get_player_bio_df()
    save_csv(player_df, 'player_bio.csv')

    team_rank_df = get_team_rank_with_match()
    save_csv(team_rank_df, 'team_rank.csv')

    save_best_players()

    match_summary_df = get_match_summary()
    save_csv(match_summary_df, 'round_robin.csv')

    ## Get 2021 per match data
    matchid_list = get_vnl_schedule_2021()
    get_matches_data_2021(matchid_list)

if __name__ == '__main__':
    main()
