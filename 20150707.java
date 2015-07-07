//TRABALHO 13

import java.util.concurrent.locks.*;

public class MCS extends Thread {
    static final int qtdFichas = 10;
    static Lock[] locks;
    static Thread[] threads;
    static final int numThreads = 5;
    int id;
    
    public MCS(int d) {id=d;}

    public static void main(String [] args){
        locks = new ReentrantLock[qtdFichas*numThreads];
        for(int i = 0; i < qtdFichas*numThreads; i++) locks[i] = new ReentrantLock();
        threads = new MCS[numThreads];
        for(int i = 0; i < numThreads; i++) threads[i] = new MCS(i);
        for(int i = 0; i < numThreads; i++) threads[i].start();
    }
    
    public void run() {
        int cont = 0;
        for(int i = 0; i < qtdFichas; i++) cont = obterFicha(cont);
    }

    public int obterFicha(int cont) {
        while(!locks[cont].tryLock()) cont++;
        System.out.println("thread " + id + " got ticket " + cont);
        return cont++;
    }
}

//TRABALHO 14
import java.util.concurrent.locks.*;

public class CSM {
    static Mulher[] mulheres;
    static Homem[] homens;
    static final int qtdMulher = 30;
    static final int qtdHomem = 30;
    
    public static int abs(int a){
        if(a < 0) return -1*a; else return a;
    }
    
    public static void main(String[] args) {
        mulheres = new Mulher[qtdMulher];
        homens = new Homem[qtdHomem];
        for(int i = 0; i < qtdMulher; i++) mulheres[i] = new Mulher();
        for(int i = 0; i < qtdHomem; i++) homens[i] = new Homem();
        for(int i = 0; i < qtdMulher; i++) mulheres[i].start();
        for(int i = 0; i < qtdHomem; i++) homens[i].start();
        for(int i = 0; i < qtdMulher; i++) {
            try{mulheres[i].join();} catch(Exception e){}
        }
        for(int i = 0; i < qtdHomem; i++) {
            try{homens[i].join();} catch(Exception e){}
        }
        
    }
    
    
}

class Mulher extends Thread {
    public void entrarMulher() {
        Banheiro.ocupar(false);
    }
    
    public void sairMulher() {
        Banheiro.desocupar(false);
    }
        
    public void run() {
        entrarMulher();
        try{this.sleep(2);} catch(Exception e){}
        sairMulher();
    }
}

class Homem extends Thread {
    public void entrarHomem() {
        Banheiro.ocupar(true);
    }
    
    public void sairHomem() {
        Banheiro.desocupar(true);
    }
    
    public void run() {
        entrarHomem();
        sairHomem();
    }
}

class Banheiro {
    static volatile int qtdPessoas = 0;
    static volatile boolean fullHomem = false;
    static volatile boolean fullMulher = false;
    static final int limit = 5;
    static int counter = 0;
    static ReentrantLock lock = new ReentrantLock();
    static Condition ocupado = lock.newCondition();
    
    public static void ocupar(boolean isHomem) {
        lock.lock();
        while((isHomem && qtdPessoas < 0) || (fullHomem)) {
            try{ocupado.await();} catch(Exception e){}
        }
        while((!isHomem && qtdPessoas > 0) || (fullMulher)) {
            try{ocupado.await();} catch(Exception e){}
        }
        if(isHomem) {
            System.out.println("Entrou homem " + (qtdPessoas+1));
            qtdPessoas++;
            counter++;
            if(counter == limit) fullHomem = true;
         }
        else {
            System.out.println("Entrou mulher" + (qtdPessoas-1));
            qtdPessoas--;
            counter++;
            if(counter == limit) fullMulher = true;
        }
        lock.unlock();
    }
    
    public static void desocupar(boolean isHomem){
        lock.lock();
        if(isHomem) {
            System.out.println("Saiu homem " + (qtdPessoas-1));
            qtdPessoas--;
        }
        else {
            System.out.println("Saiu mulher " + (qtdPessoas+1));
            qtdPessoas++;
        }
        if(qtdPessoas == 0) {
            counter = 0;
            ocupado.signalAll();
            fullHomem = false;
            fullMulher = false;
        }
        lock.unlock();
    }
    
}
