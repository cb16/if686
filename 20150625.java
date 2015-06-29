import java.util.Random;

public class Main extends Thread{
	static volatile ArvoreBusca arv = new ArvoreBusca();
	
	public Main(ArvoreBusca a) {
		this.arv = a;
	}

	public static void main(String[] args) {
		Thread[] threads = new Thread[50];
		for(int i = 0; i < 50; i++) threads[i] = new Main(arv);
		for(int i = 0; i < 50; i++) threads[i].run();
		for(int i = 0; i < 50; i++) {
			try{
				threads[i].join();
			} catch(InterruptedException e) {
				
			}
		}
		arv.preordem(arv.root);
	}
	
	public void run() {
		Random ger = new Random();
		for(int i = 0; i < 1; i++) {
			int num = ger.nextInt(100);
			arv.insert(num);
		}
	}
	
}

class ArvoreBusca {
	Node root;
	
	public ArvoreBusca() {
		root = null;
	}
	
	synchronized void insert(int val) {
		if(root == null) root = new Node(val);
		else insert2(root, val);
	}

	synchronized Node insert2(Node at, int val) {
		Node n = new Node(val);
		if(val <= at.val) {
			if(at.left == null) return at.left = n;
			else return insert2(at.left, val);
		} else {
			if(at.right == null) return at.right = n;
			else return insert2(at.right, val);
		}
	}
	
	public void preordem(Node n) {
		if(n == null) return;
		preordem(n.left);
		System.out.println("no " + n.val);
		preordem(n.right);
	}
}

class Node {
	int val;
	Node left, right;
	
	public Node(int val) {
		this.val = val;
		left = null;
		right = null;
	}
}