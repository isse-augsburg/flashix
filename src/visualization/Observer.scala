package visualization

trait Observer[-A] extends (A => Unit){
}