# USAGE: python webcrawl.py <username> <pwd>

from pyvirtualdisplay import Display
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
import sys


#display = Display(visible=0, size=(800, 600))
#display.start()


browser = webdriver.Firefox()
browser.get("https://keep.google.com/") 
time.sleep(5)
username = browser.find_element_by_id("Email")

username.send_keys(sys.argv[1])
login_attempt_1 = browser.find_element_by_xpath("//*[@type='submit']")
login_attempt_1.submit()

time.sleep(5)
password = browser.find_element_by_id("Passwd")
password.send_keys(sys.argv[2])
login_attempt_2 = browser.find_element_by_xpath("//*[@type='submit']")
login_attempt_2.submit()
#posts = browser.find_elements_by_class_name("fk-display-block")
divs = browser.find_elements_by_xpath("//*[contains(text(), 'Daily Log')]")
#time.sleep(10)
logText = ""
#counter = 0
flag = False
for div in divs:
  if flag: break
  flag = True
  #print(str(counter))
  #print (div.text)
  logText = div.text

logLines = logText.rsplit('\n')

logfile = open("../data/tolog.txt", "w")

for line in logLines:
  if not (line == "Daily Log" or line == "\n"):
    logfile.write(line)
    logfile.write("\n")

browser.quit()
# notranslate IZ65Hb-YPqjbf r4nke-YPqjbf
# notranslate IZ65Hb-YPqjbf h1U9Be-YPqjbf rTEl-SX9D7d-Y5a8lc
# IZ65Hb-TBnied HLvlvd-h1U9Be
# notranslate IZ65Hb-YPqjbf h1U9Be-YPqjbf rTEl-SX9D7d-r9oPif
# Python 2.7.10
