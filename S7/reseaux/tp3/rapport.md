# TP2

2.1. IPV6

2.2. 
```wireshark
No.     Time           Source                Destination           Protocol Length Info
      1 0.000000000    fc00:1234:1::1        fc00:1234:2::3        ICMPv6   118    Echo (ping) request id=0x0ada, seq=1, hop limit=64 (reply in 2)

Frame 1: 118 bytes on wire (944 bits), 118 bytes captured (944 bits) on interface 0
Ethernet II, Src: CadmusCo_63:a3:dd (08:00:27:63:a3:dd), Dst: CadmusCo_8f:0d:5b (08:00:27:8f:0d:5b)
Internet Protocol Version 6, Src: fc00:1234:1::1 (fc00:1234:1::1), Dst: fc00:1234:2::3 (fc00:1234:2::3)
Internet Control Message Protocol v6

No.     Time           Source                Destination           Protocol Length Info
      2 0.000686000    fc00:1234:2::3        fc00:1234:1::1        ICMPv6   118    Echo (ping) reply id=0x0ada, seq=1, hop limit=63 (request in 1)

Frame 2: 118 bytes on wire (944 bits), 118 bytes captured (944 bits) on interface 0
Ethernet II, Src: CadmusCo_8f:0d:5b (08:00:27:8f:0d:5b), Dst: CadmusCo_63:a3:dd (08:00:27:63:a3:dd)
Internet Protocol Version 6, Src: fc00:1234:2::3 (fc00:1234:2::3), Dst: fc00:1234:1::1 (fc00:1234:1::1)
Internet Control Message Protocol v6

No.     Time           Source                Destination           Protocol Length Info
      3 0.000068000    fc00:1234:1::1        fc00:1234:2::3        ICMPv6   118    Echo (ping) request id=0x0ada, seq=1, hop limit=63 (reply in 4)

Frame 3: 118 bytes on wire (944 bits), 118 bytes captured (944 bits) on interface 1
Ethernet II, Src: CadmusCo_17:9c:7e (08:00:27:17:9c:7e), Dst: CadmusCo_c8:f8:c3 (08:00:27:c8:f8:c3)
Internet Protocol Version 6, Src: fc00:1234:1::1 (fc00:1234:1::1), Dst: fc00:1234:2::3 (fc00:1234:2::3)
Internet Control Message Protocol v6

No.     Time           Source                Destination           Protocol Length Info
      4 0.000653000    fc00:1234:2::3        fc00:1234:1::1        ICMPv6   118    Echo (ping) reply id=0x0ada, seq=1, hop limit=64 (request in 3)

Frame 4: 118 bytes on wire (944 bits), 118 bytes captured (944 bits) on interface 1
Ethernet II, Src: CadmusCo_c8:f8:c3 (08:00:27:c8:f8:c3), Dst: CadmusCo_17:9c:7e (08:00:27:17:9c:7e)
Internet Protocol Version 6, Src: fc00:1234:2::3 (fc00:1234:2::3), Dst: fc00:1234:1::1 (fc00:1234:1::1)
Internet Control Message Protocol v6
```

3.3. 
```console
@vm1$ ip a
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:98:6b:de brd ff:ff:ff:ff:ff:ff
    inet6 fc00:1234:1:0:a00:27ff:fe98:6bde/64 scope global mngtmpaddr dynamic
       valid_lft 297sec preferred_lft 117sec
```
```
@vm3$ ip a
3: eth1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:98:91:1c brd ff:ff:ff:ff:ff:ff
    inet6 fc00:1234:2:0:a00:27ff:fe98:911c/64 scope global noprefixroute dynamic
       valid_lft 296sec preferred_lft 116sec
```
