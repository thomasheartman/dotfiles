# implicitly requires fish shell and the bitwarden-cli, and for the bitwarden
# session env variable ($BW_SESSION) to have been set
import subprocess

def mailpasswd(account):
  print "Starting password fetching for account %s" % account
  try:
    return subprocess.check_output('fish -c "bw get password %s" '% account, shell=True)
  except subprocess.CalledProcessError:
    return ""
