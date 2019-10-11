package Mandelbrot;

import java.awt.*;

public class Mandelbrot250000 extends Thread {
    final static int taille = 500 ;   // nombre de pixels par ligne et par colonne
    final static Picture image = new Picture(taille, taille) ;
    // Il y a donc taille*taille pixels blancs ou gris à déterminer
    final static int max = 100_000 ;
    // C'est le nombre maximum d'itérations pour déterminer la couleur d'un pixel

    static volatile boolean running = true;

    int i, j;

    public static void main(String[] args) throws InterruptedException {

        Mandelbrot250000[] trs = new Mandelbrot250000[250000];

        Thread saveImages = new Thread(new Runnable() {
            @Override
            public void run() {
                for (int i = 0; i < 1000 && running; ++i) {
                    image.save("/tmp/pic" + String.format("%03d", i) + ".png");
                    try {
                        sleep(100);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
        //saveImages.start();

        final long début = System.nanoTime() ;

        for (int i = 0; i < 250000; ++i) {
            trs[i] = new Mandelbrot250000();
            trs[i].i = i % 500;
            trs[i].j = i / 500;
            trs[i].start();
        }

        for (int i = 0; i < 4; ++i) {
            trs[i].join();
        }

        running = false;

        final long fin = System.nanoTime() ;
        final long durée = (fin - début) / 1_000_000 ;
        System.out.println("Durée = " + (double) durée / 1000 + " s.") ;
        //image.show() ;
        //saveImages.join();
    }

    @Override
    public void run() {
        colorierPixel(i, j);
    }

    // La fonction colorierPixel(i,j) colorie le pixel (i,j) de l'image en gris ou blanc
    public static void colorierPixel(int i, int j) {
        final Color gris = new Color(90, 90, 90) ;
        final Color blanc = new Color(255, 255, 255) ;
        final double xc = -.5 ;
        final double yc = 0 ; // Le point (xc,yc) est le centre de l'image
        final double region = 2 ;
        /*
          La région du plan considérée est un carré de côté égal à 2.
          Elle s'étend donc du point (xc - 1, yc - 1) au point (xc + 1, yc + 1)
          c'est-à-dire du point (-1.5, -1) en bas à gauche au point (0.5, 1) en haut
          à droite
        */
        double a = xc - region/2 + region*i/taille ;
        double b = yc - region/2 + region*j/taille ;
        // Le pixel (i,j) correspond au point (a,b)
        if (mandelbrot(a, b, max)) image.set(i, j, gris) ;
        else image.set(i, j, blanc) ; 
    }

    // La fonction mandelbrot(a, b, max) détermine si le point (a,b) est gris
    public static boolean mandelbrot(double a, double b, int max) {
        double x = 0 ;
        double y = 0 ;
        for (int t = 0; t < max; t++) {
            if (x*x + y*y > 4.0) return false ; // Le point (a,b) est blanc
            double nx = x*x - y*y + a ;
            double ny = 2*x*y + b ;
            x = nx ;
            y = ny ;
        }
        return true ; // Le point (a,b) est gris
    }
}


/* 
   $ make
   javac *.java 
   jar cvmf MANIFEST.MF Mandelbrot.jar *.class 
   manifeste ajouté
   ajout : Mandelbrot.class(entrée = 1697) (sortie = 1066)(compression : 37 %)
   ajout : Picture.class(entrée = 5689) (sortie = 3039)(compression : 46 %)
   rm *.class 
   $ java -jar Mandelbrot.jar
   Durée = 35.851 s.
   ^C
*/
