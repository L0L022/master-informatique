// -*- coding: utf-8 -*-

import java.util.ArrayDeque;
import java.util.Queue;

import static java.lang.Thread.sleep;

public class SeptNainsInterupt {
    public static void main(String[] args) throws InterruptedException {
        int nbNains = 7;
        String nom [] = {"Simplet", "Dormeur",  "Atchoum", "Joyeux", "Grincheux", "Prof", "Timide"};
        NainInt nain [] = new NainInt [nbNains];
        for(int i = 0; i < nbNains; i++) nain[i] = new NainInt(nom[i]);
        for(int i = 0; i < nbNains; i++) nain[i].start();

        sleep(5000);

        for (NainInt n : nain) {
            n.interrupt();
        }
        System.out.println("C'est fini.");
    }
}

class BlancheNeigeInt {
    private volatile boolean libre = true;        // Initialement, Blanche-Neige est libre.

    private volatile Queue<Object> queue = new ArrayDeque<>();

    public synchronized void requérir () {
        System.out.println("\t" + Thread.currentThread().getName()
                           + " veut un accès exclusif à la ressource");
        queue.add(Thread.currentThread());
    }

    public synchronized void accéder () throws InterruptedException {
        while( ! libre || ! queue.element().equals(Thread.currentThread())) { // Le nain s'endort sur l'objet bn
            wait();
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

class NainInt extends Thread {
    private static final BlancheNeigeInt bn = new BlancheNeigeInt();
    public NainInt(String nom) {
        this.setName(nom);
    }
    public void run() {
        try {
            while(true) {
                bn.requérir();
                bn.accéder();
                System.out.println("\t\t" + getName() + " possède le privilège d'accès à Blanche-Neige.");
                sleep(1000);
                bn.relâcher();
            }
        } catch (InterruptedException e) {
        }
        System.out.println(getName() + " a terminé!");
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
