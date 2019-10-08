# Configuration eth1
# RAPPEL: eth0 est à vagrant, ne pas y toucher

## Désactivation de network-manager
NetworkManager:
  service:
    - dead
    - enable: False
    
## Suppression de la passerelle par défaut
ip route del default:
  cmd:
    - run

##Configuration de VM3
eth1:
  network.managed:                                                              
    - enabled: True           
    - type: eth                                                        
    - proto: none                                                   
    - enable_ipv6: True                          
    - ipv6proto: auto
