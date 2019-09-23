# Rapport - TP1

## Physique et liaison de données
1. Il y a 18 stations.
2. Elle n'a qu’une carte réseau Ethernet.
3. Il y a 8 connecteurs électriques.
4. L’autre extrémité du câble est identique et est reliée à un autre connecteur RJ45 (mural), lui-même relié au réseau (switch).
5. La salle contient un switch.
6. C'est la même prise donc je ne pense pas.
7. MAC : 50:9a:4c:4d:0a:d6 Ipv4 : 139.124.75.159

```console
$ ip addr
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
2: enp0s31f6: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UP group default qlen 1000
    link/ether 50:9a:4c:4d:0a:d6 brd ff:ff:ff:ff:ff:ff
    inet 139.124.75.159/24 brd 139.124.75.255 scope global dynamic enp0s31f6
       valid_lft 12578sec preferred_lft 12578sec
```

## Compte-rendu
2. Elle a 3 interfaces réseaux : l’interface locale (lo), et deux interfaces
ethernet (la première étant mise en place par défaut par vagrant).
3. 
```console
$ ip addr
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:f8:33:b7 brd ff:ff:ff:ff:ff:ff
    inet 10.0.2.15/24 brd 10.0.2.255 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fef8:33b7/64 scope link 
       valid_lft forever preferred_lft forever
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:7d:7f:ae brd ff:ff:ff:ff:ff:ff
    inet6 fe80::a00:27ff:fe7d:7fae/64 scope link 
       valid_lft forever preferred_lft forever
```
4. Il est possible de se connecter à internet via l’interface réseau créée automatiquement par vagrant, dans la mesure où l’accès aux serveurs DNS à été établi préalablement (il est parfois nécessaire de lancer le service DHCP manuellement).
5. On lance ```vagrant halt``` depuis l'hôte.
8. 
```console
$ ip addr
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:f8:33:b7 brd ff:ff:ff:ff:ff:ff
    inet 10.0.2.15/24 brd 10.0.2.255 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fef8:33b7/64 scope link 
       valid_lft forever preferred_lft forever
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:7d:7f:ae brd ff:ff:ff:ff:ff:ff
    inet6 fe80::a00:27ff:fe7d:7fae/64 scope link 
       valid_lft forever preferred_lft forever
4: eth2: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:3b:17:7d brd ff:ff:ff:ff:ff:ff
    inet6 fe80::a00:27ff:fe3b:177d/64 scope link 
       valid_lft forever preferred_lft forever
```

## Nouvelles VMs
1. Hôte <-> VM impossible. VM <-> VM ok.
2. On peut accéder à internet grâce à l'interface mise en place par vagrant.
3. 
```console
@bleu$ ip addr
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:f8:33:b7 brd ff:ff:ff:ff:ff:ff
    inet 10.0.2.15/24 brd 10.0.2.255 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fef8:33b7/64 scope link 
       valid_lft forever preferred_lft forever
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:0b:9a:69 brd ff:ff:ff:ff:ff:ff
    inet6 fe80::a00:27ff:fe0b:9a69/64 scope link 
       valid_lft forever preferred_lft forever
```

```console
@verte$ ip addr
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:f8:33:b7 brd ff:ff:ff:ff:ff:ff
    inet 10.0.2.15/24 brd 10.0.2.255 scope global eth0
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fef8:33b7/64 scope link 
       valid_lft forever preferred_lft forever
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:7f:5a:08 brd ff:ff:ff:ff:ff:ff
    inet6 fe80::a00:27ff:fe7f:5a08/64 scope link 
       valid_lft forever preferred_lft forever
```

5. Paramètres nécessaires: adresse IP, masque, MAC, passerelle. Optionnels: serveur DNS.

## Observation du Trafic Réseau
3. Mise en évidence du protocole ARP (requête / réponse), extrait sous format texte de WireShark :
```wireshark
No.     Time           Source                Destination           Protocol Length Info
17      43.268202000   CadmusCo_0b:9a:69     Broadcast             ARP      60     Who has 192.168.1.2?  Tell 192.168.1.1

Frame 17: 60 bytes on wire (480 bits), 60 bytes captured (480 bits) on interface 0
Ethernet II, Src: CadmusCo_0b:9a:69 (08:00:27:0b:9a:69), Dst: Broadcast (ff:ff:ff:ff:ff:ff)
Address Resolution Protocol (request)

No.     Time           Source                Destination           Protocol Length Info
18      43.268267000   CadmusCo_7f:5a:08     CadmusCo_0b:9a:69     ARP      42     192.168.1.2 is at 08:00:27:7f:5a:08

Frame 18: 42 bytes on wire (336 bits), 42 bytes captured (336 bits) on interface 0
Ethernet II, Src: CadmusCo_7f:5a:08 (08:00:27:7f:5a:08), Dst: CadmusCo_0b:9a:69 (08:00:27:0b:9a:69)
Address Resolution Protocol (reply)

No.     Time           Source                Destination           Protocol Length Info
31      48.270922000   CadmusCo_7f:5a:08     CadmusCo_0b:9a:69     ARP      42     Who has 192.168.1.1?  Tell 192.168.1.2

Frame 31: 42 bytes on wire (336 bits), 42 bytes captured (336 bits) on interface 0
Ethernet II, Src: CadmusCo_7f:5a:08 (08:00:27:7f:5a:08), Dst: CadmusCo_0b:9a:69 (08:00:27:0b:9a:69)
Address Resolution Protocol (request)

No.     Time           Source                Destination           Protocol Length Info
32      48.271302000   CadmusCo_0b:9a:69     CadmusCo_7f:5a:08     ARP      60     192.168.1.1 is at 08:00:27:0b:9a:69

Frame 32: 60 bytes on wire (480 bits), 60 bytes captured (480 bits) on interface 0
Ethernet II, Src: CadmusCo_0b:9a:69 (08:00:27:0b:9a:69), Dst: CadmusCo_7f:5a:08 (08:00:27:7f:5a:08)
Address Resolution Protocol (reply)
```