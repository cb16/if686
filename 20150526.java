
public class IncrementarContador implements Runnable {
	int me;
	int limit;
	public IncrementarContador(int a, int l){
		me = a;
		limit = l;
	}
	public static void main(String[] args) {
		for(int i = 0; i < 5; i++){
			(new Thread(new IncrementarContador(i,20))).start();
		}
	}
	
	public void run(){
		for(int i = 0; i < limit; i++){
			System.out.println("thread " + me + " = " + i);
		}
	}
}

class FirstClass implements Runnable {
	int left, right;
	static int value = 1000000000;
	public FirstClass(int a, int b) {
		left = a;
		right = b;
	}
	
	public static void main(String[] args) {
		double startTime = System.currentTimeMillis();
		for(int i = 0; i<5; i++) {
			Thread a =new Thread (new FirstClass(i*(value/5), (i+1)*(value/5))); 
			a.start();
			try {
				a.join();
			} catch (InterruptedException e) {
				System.out.println("erro!");
			}
		}
		double finishTime = System.currentTimeMillis();
		System.out.println("TIME: " + (finishTime - startTime));
	}
	
	public void run() {
		for(int i = left; i<right; i++) {
			System.out.println(i);
		}
	}
}