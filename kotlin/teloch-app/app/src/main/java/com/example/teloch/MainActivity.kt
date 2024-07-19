package com.example.teloch

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Slider
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.tooling.preview.Preview
import com.example.teloch.ui.theme.TelochTheme
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetAddress
import java.nio.ByteBuffer
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import kotlin.concurrent.thread


const val serverAddress = "192.168.1.249"
const val serverPort = 27015

fun floatToByteArray(f: Float): ByteArray {
    val byteBuffer = ByteBuffer.allocate(4).putFloat(f)
    val byteArray = byteBuffer.array()
    byteArray.reverse()
    return byteArray
}

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        val queue: BlockingQueue<Float> = LinkedBlockingQueue()

        thread {
            try {
                val socket = DatagramSocket()
                val address = InetAddress.getByName(serverAddress)
                while (true) {
                    val v = queue.take()
                    socket.send(
                        DatagramPacket(floatToByteArray(v), 4, address, serverPort)
                    )
                }
            } catch (e: Exception) {
                e.printStackTrace()
            }
        }

        super.onCreate(savedInstanceState)
        setContent {
            TelochTheme {
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colorScheme.background
                ) {
                    Volume(queue)
                }
            }
        }
    }
}


@Composable
fun Volume(queue: BlockingQueue<Float>) {
    var sliderPosition by remember { mutableFloatStateOf(0f) }
    Text(
        text = (sliderPosition * 100).toString()
    )
    Box(
        contentAlignment = Alignment.Center,
        modifier = Modifier.fillMaxSize()
    ) {
        Slider(
            value = sliderPosition,
            onValueChange = {
                sliderPosition = it
                queue.add(it)
            },
            steps = 100,
            valueRange = 0f..1f,
            modifier = Modifier.rotate(-90f)
        )
    }
}

@Preview(showBackground = true)
@Composable
fun VolumePreview() {
    val queue: BlockingQueue<Float> = LinkedBlockingQueue()
    TelochTheme {
        Volume(queue)
    }
}