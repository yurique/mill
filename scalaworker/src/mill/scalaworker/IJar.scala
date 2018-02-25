package mill.scalaworker
import org.objectweb.asm.Opcodes.ASM6
import org.objectweb.asm._
class MethodReplacer(val cw: ClassVisitor) extends ClassVisitor(ASM6, cw) {

  override def visitAttribute(attr: Attribute): Unit = {
    cv.visitAttribute(attr)
  }

  override def visitField(access: Int,
                          name: String,
                          desc: String,
                          signature: String,
                          value: Any) = {
    if ((access & Opcodes.ACC_PRIVATE) != 0) null
    else cv.visitField(access, name, desc, signature, value)
  }

  override def visitMethod(access: Int,
                           name: String,
                           desc: String,
                           signature: String,
                           exceptions: Array[String]) = {
    if ((access & Opcodes.ACC_PRIVATE) != 0) {
      println("Skip Method " + name)
      null
    }
    else {
      println("Visit Method " + name)
      new ReplaceWithEmptyBody(
        super.visitMethod(access, name, desc, signature, exceptions),
        (Type.getArgumentsAndReturnSizes(desc) >> 2) - 1
      )
    }
  }
}

class ReplaceWithEmptyBody(val targetWriter: MethodVisitor, val newMaxLocals: Int)
  extends MethodVisitor(Opcodes.ASM6) {
  // we're only override the minimum to create a code attribute with a sole RETURN
  override def visitMaxs(maxStack: Int, maxLocals: Int): Unit = {
    targetWriter.visitMaxs(0, newMaxLocals)
  }

  override def visitCode(): Unit = {
    targetWriter.visitCode()
    targetWriter.visitInsn(Opcodes.RETURN) // our new code

  }

  override def visitEnd(): Unit = {
    targetWriter.visitEnd()
  }

  // the remaining methods just reproduce meta information,
  // annotations & parameter names
  override def visitAnnotation(desc: String, visible: Boolean) = {
    targetWriter.visitAnnotation(desc, visible)
  }

  override def visitParameter(name: String, access: Int): Unit = {
    targetWriter.visitParameter(name, access)
  }
}