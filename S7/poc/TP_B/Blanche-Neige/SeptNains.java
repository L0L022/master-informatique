// -*- coding: utf-8 -*-

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Queue;

public class SeptNains {
    public static void main(String[] args) {
        int nbNains = 7;
        String nom [] = {"Simplet", "Dormeur",  "Atchoum", "Joyeux", "Grincheux", "Prof", "Timide"};
        Nain nain [] = new Nain [nbNains];
        for(int i = 0; i < nbNains; i++) nain[i] = new Nain(nom[i]);
        for(int i = 0; i < nbNains; i++) nain[i].start();
        for(int i = 0; i < nbNains; i++) {
            try { nain[i].join(); } catch (InterruptedException e) {e.printStackTrace();}	
        }
        // System.out.println("C'est fini.");        
    }
}    

class BlancheNeige {
    private volatile boolean libre = true;        // Initialement, Blanche-Neige est libre.

    private volatile Queue<Object> queue = new ArrayDeque<>();

    public synchronized void requérir () {
        System.out.println("\t" + Thread.currentThread().getName()
                           + " veut un accès exclusif à la ressource");
        queue.add(Thread.currentThread());
    }

    public synchronized void accéder () {
        while( ! libre || ! queue.element().equals(Thread.currentThread())) { // Le nain s'endort sur l'objet bn
            try { wait(); } catch (InterruptedException e) {e.printStackTrace();}
        }
        queue.remove();
        libre = false;
        System.out.println("\t\t" + Thread.currentThread().getName()
                           + " va accéder à la ressource.");
    }

    public synchronized void relâcher () {
        System.out.println("\t\t\t" + Thread.currentThread().getName()
                           + " relâche la ressource.");
        libre = true;
        notifyAll();
    }
}

class Nain extends Thread {
    private static final BlancheNeige bn = new BlancheNeige();
    public Nain(String nom) {
        this.setName(nom);
    }
    public void run() {
        while(true) {
            bn.requérir();
            bn.accéder();
            System.out.println("\t\t" + getName() + " possède le privilège d'accès à Blanche-Neige.");
            try {sleep(1000);} catch (InterruptedException e) {e.printStackTrace();}
            bn.relâcher();
        }
        // System.out.println(getName() + " a terminé!");
    }	
}

/*
  $ make
  $ $ java SeptNains
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
	Grincheux veut un accès exclusif à la ressource
	Timide veut un accès exclusif à la ressource
	Joyeux veut un accès exclusif à la ressource
	Atchoum veut un accès exclusif à la ressource
	Dormeur veut un accès exclusif à la ressource
	Prof veut un accès exclusif à la ressource
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
			Simplet relâche la ressource.
	Simplet veut un accès exclusif à la ressource
		Simplet va accéder à la ressource.
		Simplet possède le privilège d'accès à Blanche-Neige.
        ...
*/
