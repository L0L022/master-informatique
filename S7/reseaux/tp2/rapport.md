2. nb sous réseaux 2**(32-25)
3. nb machines par sous réseaux 2**(32-25-4)
4. 
premier sous-réseau :

10101100 00010000 00000010 10000100 172.16.2.132
&
11111111 11111111 11111111 11110000 172.16.2.240
=
10101100 00010000 00000010 10000000 172.16.2.128

masque réseau :
11111111 11111111 11111111 11110000 172.16.2.240

broadcast :
10101100 00010000 00000010 10001111 172.16.2.143

second sous-réseau :

10101100 00010000 00000010 10100010 172.16.2.162
&
11111111 11111111 11111111 11110000 172.16.2.240
=
10101100 00010000 00000010 10100000 172.16.2.160

masque réseau :
11111111 11111111 11111111 11110000 172.16.2.240

broadcast :
10101100 00010000 00000010 10101111 172.16.2.175

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
    link/ether 08:00:27:7d:22:47 brd ff:ff:ff:ff:ff:ff
    inet 172.16.2.132/28 brd 172.16.2.143 scope global eth1
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fe7d:2247/64 scope link 
       valid_lft forever preferred_lft forever
4: eth2: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:81:cb:39 brd ff:ff:ff:ff:ff:ff
    inet 172.16.2.162/28 brd 172.16.2.175 scope global eth2
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fe81:cb39/64 scope link tentative 
       valid_lft forever preferred_lft forever
```

```
No.     Time           Source                Destination           Protocol Length Info
      1 0.000000000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=1/256, ttl=64 (no response found!)

Frame 1: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      2 0.000030000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=1/256, ttl=63 (reply in 3)

Frame 2: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      3 0.000721000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=1/256, ttl=64 (request in 2)

Frame 3: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      4 0.000741000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=1/256, ttl=63

Frame 4: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      5 1.001179000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=2/512, ttl=64 (no response found!)

Frame 5: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      6 1.001210000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=2/512, ttl=63 (reply in 7)

Frame 6: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      7 1.001946000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=2/512, ttl=64 (request in 6)

Frame 7: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      8 1.001969000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=2/512, ttl=63

Frame 8: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
      9 2.002286000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=3/768, ttl=64 (no response found!)

Frame 9: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
     10 2.002314000    172.16.2.131          172.16.2.163          ICMP     100    Echo (ping) request  id=0x069a, seq=3/768, ttl=63 (reply in 11)

Frame 10: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.131 (172.16.2.131), Dst: 172.16.2.163 (172.16.2.163)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
     11 2.002978000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=3/768, ttl=64 (request in 10)

Frame 11: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
     12 2.003001000    172.16.2.163          172.16.2.131          ICMP     100    Echo (ping) reply    id=0x069a, seq=3/768, ttl=63

Frame 12: 100 bytes on wire (800 bits), 100 bytes captured (800 bits) on interface 0
Linux cooked capture
Internet Protocol Version 4, Src: 172.16.2.163 (172.16.2.163), Dst: 172.16.2.131 (172.16.2.131)
Internet Control Message Protocol

No.     Time           Source                Destination           Protocol Length Info
     13 5.001023000    CadmusCo_c9:10:d4                           ARP      62     Who has 172.16.2.162?  Tell 172.16.2.163

Frame 13: 62 bytes on wire (496 bits), 62 bytes captured (496 bits) on interface 0
Linux cooked capture
Address Resolution Protocol (request)
VSS-Monitoring ethernet trailer, Source Port: 0

No.     Time           Source                Destination           Protocol Length Info
     14 5.001062000    CadmusCo_ca:37:67                           ARP      44     172.16.2.162 is at 08:00:27:ca:37:67

Frame 14: 44 bytes on wire (352 bits), 44 bytes captured (352 bits) on interface 0
Linux cooked capture
Address Resolution Protocol (reply)

No.     Time           Source                Destination           Protocol Length Info
     15 5.014638000    CadmusCo_e9:8c:9f                           ARP      44     Who has 172.16.2.131?  Tell 172.16.2.132

Frame 15: 44 bytes on wire (352 bits), 44 bytes captured (352 bits) on interface 0
Linux cooked capture
Address Resolution Protocol (request)

No.     Time           Source                Destination           Protocol Length Info
     16 5.015339000    CadmusCo_09:71:ef                           ARP      62     172.16.2.131 is at 08:00:27:09:71:ef

Frame 16: 62 bytes on wire (496 bits), 62 bytes captured (496 bits) on interface 0
Linux cooked capture
Address Resolution Protocol (reply)
VSS-Monitoring ethernet trailer, Source Port: 0
```